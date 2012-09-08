{
  rzfswizard
  x1nixmzeng (July 2012)

  July 21st
  Added data checksum (although unchecked in client)
  
  July 15th
  Added some structure comments and function descriptions
}
unit rzFileSys;

interface

// Optional debugging features
//
{ $DEFINE EXPORT_MSFDEC}   // Dump the decompressed MSF data
{ $DEFINE EXPORT_FILELIST} // Save a plaintext file with the filenames
{ $DEFINE EXPORT_MSFTEST}  // Dump the new MSF data

uses
  Dialogs, ComCtrls, SysUtils, Classes, libMSF,
  ULZMADecoder, ULZMAEncoder;

type
  uint32 = Cardinal;
  uint16 = Word;

  (***************************************
    Static MSF file entry structure
    20-bytes (2 bytes of padding)
  ****************************************)
  FILEINDEX_ENTRY = packed record
    size,      // Uncompressed filesize
    offset,    // Data offset in MRF
    zsize      // Compressed filesize in MRF file
             : uint32;
    lenMRFN,   // Length of MRF filename
    lenName,   // Length of this filename
    dataHash,  // Uncompressed data checksum
    _padding_  // Structure alignment
             : uint16;
  end;

  (***************************************
    MRF entry as used by rzfswizard
    36-bytes
  ****************************************)
  MSF_ENTRY = record
    mrfOwner,   // Index to MrfFiles tstringlist
    mrfIndex,   // Part number (0 == '.mrf', 1 == '.001', etc)
    fileIndex   // Index to FileList tstringlist
             : uint32;
    entryData   // Raw fileindex data
             : FILEINDEX_ENTRY;
    replaceW    // Path to file replacement (when not empty '')
             : String;
  end;

  (***************************************
    MSF class to handle data inside fileindex.msf
   ***************************************)
  type MSF = class
  private
    MrfFiles,  // Array of unique MRF files
    FileList   // Array of each filename in the system (large!)
             : TStringList;
    FileEntries// Raw static MRF info (stored this way for exporting)
             : array of MSF_ENTRY;
    Files      // Actual number of files in the system (use FileList count?)
             : uint32;

    function AddMrfFile( str : string ) : Cardinal;
    function GetMrfIndex( str : string) : Cardinal;
    procedure ExportIndex( FIndex: TMemoryStream );    
    procedure ImportIndex( FIndex: TMemoryStream );

  public
    constructor Create;
    destructor Destroy; override;
    procedure CleanUp;

    function GetMrfPartName( index: cardinal ): string;

    function SaveFileIndex( FileName: String ) : Boolean;    
    function LoadFileIndex( FileName: String ) : Boolean;
    function FileCount() : Integer;

    function GetMrfList() : TStringList;

    function GetMrfPartCount(mrfIndex2: Cardinal): Cardinal;
    function GetFilesForMrf(mrfIndex: Cardinal): Cardinal;
    function GetTotalSizeForMrf(mrfIndex: Cardinal): Cardinal;
    function GetTotalZSizeForMrf(mrfIndex: Cardinal): Cardinal;

    procedure AddFilesToList( grid: TListItems; mrfIndex: cardinal );
    procedure ReplaceFile( mrfIndex, fileIndex: Cardinal; const fName: String );
    procedure InsertFile( mrfIndex : Cardinal; const fName: String );

    function CountFileReplacements(): Cardinal; // did return stringlist
  end;

  (***************************************
    Class to handle MRF patching
   ***************************************)
  MSFPatcher = class(TThread)
    public
      fpacked: TMemoryStream;
    private
      myStatus: integer;
      msfi    : ^MSF;
      log     : TStringList;
      isOver  : Boolean;
    protected
      procedure Execute; override;
    public
      constructor Create;
      destructor Destroy;
      procedure SetMSF(var msfindex: MSF);

      function finishedPatch(): Boolean;
      function getProgress: integer;
      function getLogMessages(): TStringList;
  end;

  function UnLZMA(inFile: TMemoryStream; outSize:Cardinal): TMemoryStream;
  function PackLZMA(inFile: TMemoryStream): TMemoryStream;

implementation

// Locate the file extension from a filename
//
function StripExt(From:String):String;
begin
  Result:= Copy(From, 0, Length(From)-Length(ExtractFileExt(From)));
end;

// MSF class constructor
//
constructor MSF.Create;
begin
  inherited Create;

  // Make hash table if it does not exist
  if not libMSF.msfGetHashTable() then
  begin
    libMSF.msfMakeHashTable();
  end;

  // Setup array
  SetLength(FileEntries, 0);
  Files    := 0;
end;

// Destroy allocated classes
//
procedure MSF.CleanUp;
begin
  // Destroy string arrays
  if MrfFiles <> nil then MrfFiles.Clear;
  MrfFiles.Free;
  if FileList <> nil then FileList.Clear;
  FileList.Free;

  // Clear array
  SetLength(FileEntries, 0);
  Files    := 0;
end;

// MSF class destructor
//
destructor MSF.Destroy;
begin
  CleanUp;

  // Destroy base class
  inherited Destroy;
end;

// Add unique MRF names into the MSF filename array
//   The built-in duplicate methods are not used on purpose! 
function MSF.AddMrfFile( str : string ) : Cardinal;
var i: integer;
begin

  // Copy the filename without the extension (if there is one) 
  str := Copy(str, 0, length(str)-length(Extractfileext(str)));

  // Starting from the back, check the filename has been previously added
  for i := MrfFiles.Count downto 1 do
  begin
    if MrfFiles.Strings[i-1] = str then
    begin
      // Filename has already been added, so return the index and exit
      Result := i-1;
      Exit;
    end;
  end;

  // Filename does not exist, so push it back (add() returns the index)
  Result := MrfFiles.Add( str );

end;

// Determine the MRF part index from a filename
//
function MSF.GetMrfIndex( str: string ) : Cardinal;
var
  i: integer;
const
  MRF_NOT_SPLIT  = 0;
begin

  // Initially not split (where extension = '.mrf')
  Result := MRF_NOT_SPLIT;
  str    := ExtractFileExt(str);

  if ( length(Str) > 0 ) and ( str <> '.mrf' ) then
  begin
    i := 1; // start past the leading period

    // atoi :
    while ( i < length(str) ) and ( str[i+1] in ['0'..'9'] ) do
    begin
      Result := 10 * Result + (ord(str[i+1])-48);
      inc(i);
    end;
  end;

end;

// Parse the unpacked fileindex.msf data
//
procedure MSF.ImportIndex( FIndex: TMemoryStream );
var
  fRec : FILEINDEX_ENTRY;
  fn, mrfn: string;
begin

  // todo: strip any existing data so another index can be reloaded

  Files := 0;

  // First, loop through and determine the file count
  // (this is much faster than re-allocating memory 25K times)
  //
  while (FIndex.Position < FIndex.Size) do
  begin
    FIndex.Read( fRec, SizeOf( FILEINDEX_ENTRY ) );
    FIndex.Seek( fRec.lenMRFN + fRec.lenName, soCurrent );
    inc(Files);
  end;

  // Allocate the array size
  SetLength(FileEntries, Files);

  FIndex.Position := 0;
  Files := 0;
  
  // Then loop through reading each entry
  while (FIndex.Position < FIndex.Size) do
  begin
    with FileEntries[Files] do
    begin
      // Read structures:
      // #1
      FIndex.Read( entryData, SizeOf(FILEINDEX_ENTRY) );

      // #2 (MRF filename string)
      SetLength(mrfn, entryData.lenMRFN);
      FIndex.Read(pchar(mrfn)^, entryData.lenMRFN);
      
      // #3 (filename string)
      SetLength(fn, entryData.lenName);
      FIndex.Read(pchar(fn)^, entryData.lenName);

      // Store data:
      // All filenames are unique, so get their own entry
      FileList.Add(fn);

      // Update remaining FileEntries properties
      fileIndex:= FileList.Count-1;
      mrfOwner := AddMrfFile(mrfn);
      mrfIndex := GetMrfIndex(mrfn);
      replaceW := '';

      Inc(Files);
    end;

  end;

  {$IFDEF EXPORT_FILELIST}
    // Log the unique filenames to file
    FileList.SaveToFile('filelist.txt');
  {$ENDIF}
end;

// Unpack the MSF data from file
//
function MSF.LoadFileIndex( FileName: String ) : Boolean;
var
  FIndex   : TFileStream;
  unSize   : Cardinal;

  UnpackBuf,
  tmpBuffer: TMemoryStream;

begin

  Result := False;

  // Check the file exists
  if not FileExists( FileName ) then Exit;

  // Open the file for reading
  FIndex := TFileStream.Create( FileName, fmOpenRead );

  // Get the uncompressed size
  FIndex.Read(unSize, 4);

  // Copy this data to its own buffer
  UnpackBuf := TMemoryStream.Create;
  UnpackBuf.CopyFrom(FIndex, FIndex.Size - 4);
  FIndex.Free;

  // Unscramble the data
  UnpackBuf.Position := 0;
  libMSF.msfUnscramble(Byte(UnpackBuf.Memory^), UnpackBuf.Size);

  // Unpack in memory
  tmpBuffer := UnLZMA(UnpackBuf, unSize);
  UnpackBuf.Free;

  // NEW Check to avoid attempting to read invalid data
  // Check expected size
  if tmpBuffer.Size <> unSize then
  begin
    tmpBuffer.Free;
    Result := false;
    Exit;
  end;

  // Import the file data
  tmpBuffer.Position := 0;
  {$IFDEF EXPORT_MSFDEC}
    tmpBuffer.SaveToFile('flist_debug.msf');
  {$ENDIF}

  if tmpBuffer.Size > 0 then
  begin
    // Destroy existing data
    CleanUp;

    // Create the string arrays
    MrfFiles := TStringList.Create;
    FileList := TStringList.Create;

    ImportIndex( tmpBuffer );
  end;

  Result := ( tmpBuffer.Size > 0 );

  // Clear the buffers
  tmpBuffer.Free;

end;

// Get the MRF extension based on the part index
//
function MSF.GetMrfPartName( index: cardinal ): string;
begin
  if index = 0 then result := '.mrf'
  else              result := format('.%.3d',[index]);
end;

// Create the fileindex.msf data structure
//
procedure MSF.ExportIndex( FIndex: TMemoryStream );
var
  i: Cardinal;
  tmp: string;
begin

  for i:=1 to Files do
  begin
    // Write the stored FILEINDEX_ENTRY structure
    FIndex.Write( FileEntries[i-1].entryData, sizeof( FILEINDEX_ENTRY ) );

    // Write the MRF filename:
    // #1 Find the filename (TODO: CHECK LENGTH)
    tmp := MrfFiles[ FileEntries[i-1].mrfOwner ];
    // #2 Get the file extension
    tmp := tmp + GetMrfPartName( FileEntries[i-1].mrfIndex );
    // #3 Write the string
    FIndex.Write( tmp[1], length(tmp) );

    // Write the filename:
    // #1 Get the string (TODO: CHECK LENGTH)
    tmp := FileList.Strings[i-1];
    // #2 Write the string
    FIndex.Write( tmp[1], length(tmp) );
  end;

  // Housekeeping!
  tmp := '';

end;

// Pack the MSF data from file TODO
//
function MSF.SaveFileIndex( FileName: String ) : Boolean;
var
  FIndex : TMemoryStream; // output file
  tmpBuffer1, tmpBuffer2: TMemoryStream;
  unpack_size: cardinal;
begin

  tmpBuffer1 := TMemoryStream.Create;

  // Create the fileindex.msf structure
  ExportIndex( tmpBuffer1 );

  {$IFDEF EXPORT_MSFTEST}
    tmpBuffer1.SaveToFile('msf_rebuild.msf');
  {$ENDIF}

  // Save the raw filesize
  unpack_size := tmpBuffer1.Size;

  // Pack in memory
  tmpBuffer2 := PackLZMA( tmpBuffer1 );
  tmpBuffer1.Free;

  // Scramble the data
  tmpBuffer2.Position := 0;
  libMSF.msfScramble(Byte(tmpBuffer2.Memory^), tmpBuffer2.Size);

  FIndex := TMemoryStream.Create;

  // Write unpacked size
  FIndex.Write(unpack_size, 4);
  // Write compressed+scrambled data
  FIndex.CopyFrom(tmpBuffer2, tmpBuffer2.Size);
  
  tmpBuffer2.Free;

  // Save packed data
  FIndex.SaveToFile( FileName );

  FIndex.Free;
  Result := True;

end;

// Count the number of files in the system
//
function MSF.FileCount : Integer;
begin
  Result := Files;
end;

// Return the unique MRF filenames
//
function MSF.GetMrfList() : TStringList;
begin
  Result := MrfFiles;
end;

// Count the number of parts from a MRF filename list
//
function MSF.GetMrfPartCount(mrfIndex2: Cardinal): Cardinal;
var i: Cardinal;
begin

  Result := 0;
  for i:=1 to Files do
    with FileEntries[i-1] do
      if ( mrfOwner = mrfIndex2 ) and( mrfIndex > Result ) then
        Result := mrfIndex;

  Inc(Result);

end;

// Count the files inside a MRF from a MRF filename index
//
function MSF.GetFilesForMrf(mrfIndex: Cardinal): Cardinal;
var i: Cardinal;
begin
  Result := 0;
  for i:=1 to Files do
    if FileEntries[i-1].mrfOwner = mrfIndex then
      inc(Result);
end;

// Count the total filesize of files inside a MRF
//
function MSF.GetTotalSizeForMrf(mrfIndex: Cardinal): Cardinal; 
var i: Cardinal;
begin
  Result := 0;
  for i:=1 to Files do
    if FileEntries[i-1].mrfOwner = mrfIndex then
      inc( Result, FileEntries[i-1].entryData.size );
end;

// Count the total compressed size of files inside a MRF
//
function MSF.GetTotalZSizeForMrf(mrfIndex: Cardinal): Cardinal;
var i: Cardinal;
begin
  Result := 0;
  for i:=1 to Files do
    if FileEntries[i-1].mrfOwner = mrfIndex then
      inc( Result, FileEntries[i-1].entryData.zsize );
end;

// Export the file list from MRF index
//
procedure MSF.AddFilesToList( grid: TListItems; mrfIndex: cardinal );
var i: Cardinal;

function IntToSize( int: Cardinal ): String;
var i: integer;
const TAGS : array[0..3] of string =
  (
    ' b',
    ' Kb',
    ' Mb',
    ' Gb'  // fixed the typo here ( was Tb ! )
  );
begin

  i := 0;

  while int > 1000 do
  begin
    int := int div 1000;
    inc(i);
  end;

  Result := IntToStr( int ) + TAGS[i];

end;

begin
  for i:=1 to Files do
    if FileEntries[i-1].mrfOwner = mrfIndex then
    begin

      with grid.Add do
      begin
        {
         NAME
         SIZE
         REPLACEMENT
        }
        Caption := FileList[i-1];
        
        SubItems.Add(IntToSize(FileEntries[i-1].entryData.size));
        if length(FileEntries[i-1].replaceW) > 0 then
          SubItems.Add( FileEntries[i-1].replaceW )
        else
          SubItems.Add('<none>');

      end;

    end;
end;

// Mark a file for replacement
//
procedure MSF.ReplaceFile( mrfIndex, fileIndex: Cardinal; const fName:String );
var
  i,fc: cardinal;
begin

  fc:=0;

  for i:=1 to Files do
    if FileEntries[i-1].mrfOwner = mrfIndex then
    begin
      if( fc = fileIndex ) then
      begin
        FileEntries[i-1].replaceW := fName;
        Exit;
      end;

      inc(fc);
    end;

end;

procedure MSF.InsertFile( mrfIndex : Cardinal; const fName: String );
begin
      {
  Inc(Files);
  SetLength(FileEntries, Files);

  FileEntries[Files-1].mrfOwner := mrfIndex;
  FileEntries[Files-1].mrfIndex := 0; // store in first part
  FileEntries[Files-1].fileIndex:= Files;

  FileEntries[Files-1].entryData.


  // .. and mark for replacement, so it gets copied
  FileEntries[Files-1].replaceW := fName;
   }
end;

// Count the number of marked files for replacement
//
function MSF.CountFileReplacements(): Cardinal;
var i: integer;
begin
  Result := 0;

  for i:=1 to Files do
    if FileEntries[i-1].replaceW <> '' then
      inc(Result);

end;

// Unpack a file in memory
//
function UnLZMA(inFile: TMemoryStream; outSize:Cardinal): TMemoryStream;
var
  decoder:TLZMADecoder;
begin

  Result := TMemoryStream.Create;

  decoder := TLZMADecoder.Create;

  try
    decoder.SetDictionarySize( $10000 ); // 2^16
    decoder.SetLcLpPb(3,0,2);
    decoder.Code(inFile, Result, outSize);
  finally
    decoder.Free;
  end;

end;

// Pack file in memory
//
function PackLZMA(inFile: TMemoryStream): TMemoryStream;
var
  encoder:TLZMAEncoder;

begin
  encoder := TLZMAEncoder.Create;

  encoder.SetDictionarySize($10000); // 2^16
  encoder.SetLcLpPb(3,0,2);

  Result := TMemoryStream.Create;

  inFile.Position :=0;
  encoder.Code(inFile, Result, inFile.Size, -1);

  encoder.Free;

end;



constructor MSFPatcher.Create;
begin
  inherited Create(True);

  // Create log list
  log:=TStringList.Create();
  isOver := false;
end;

destructor MSFPatcher.Destroy;
begin
  // Null pointer value
  if msfi <> nil then msfi := nil;

  // Free log
  log.Free;
end;

procedure MSFPatcher.SetMSF(var msfindex: MSF);
begin
  msfi:=@msfindex;
end;

{
  TODO

  MSFPatcher procedure which can make the file changes from the same MSF part #
  
}


procedure MSFPatcher.Execute;
var
  i,j,
  deltaSize : integer;
  checksum : word;
  plist: array of integer;
  mrf,
  base : string;

  filesize,
  ofilesize :cardinal;
  tmp, lzfile:TMemoryStream;

  fs1, fs2 : TFileStream;

  entry, entry2 : ^MSF_ENTRY;
begin
  assert( msfi <> nil );

  isOver := False;

  // Reset status
  myStatus := 0;

  // We want an array of file indexes to patch 
  SetLength(plist, 0);

  // Make the array of file replacement indexes
  j:=0;
  for i:=1 to msfi.FileCount do
  begin
    if msfi.FileEntries[i-1].replaceW <> '' then
    begin
      // todo: check file still exists (maybe later down)
      SetLength(plist, j+1);
      plist[j] := i-1;
      inc(j);
    end;
  end;

  // Ensure we're at the fileindex.msf directory from tab #1
  base := GetCurrentDir();
  if base[length(base)-1] <> '\' then base := base + '\';

  // Log the current directory
  log.Add('Filesystem base: "'+base+'"');

  // For each marked replacement
  for i:=1 to j do
  begin
    entry := @(msfi.FileEntries[ plist[i-1] ]);

    // Check replacement still exists
    if not fileexists( entry.replaceW ) then
    begin
      log.Add('ERROR: Unable to open "'+entry.replaceW+'"');
      entry := nil;
    end;

    if entry <> nil then
    begin   
      // Make MRF container name
      mrf := msfi.MrfFiles[ entry.mrfOwner ] + msfi.GetMrfPartName( entry.mrfIndex );

      // Check MRF exists in base system
      mrf := base + mrf;

      // todo: update slashes

      if not fileexists( mrf ) then
      begin
        log.Add('ERROR: Unable to open "'+mrf+'"');
        entry := nil;
      end;
    end;

    if entry <> nil then
    begin

      log.Add('Compressing '+entry.replaceW + '..');

      // Load the new file
      tmp := TMemoryStream.Create;
      tmp.LoadFromFile( entry.replaceW );

      // Handle the filesize
      ofilesize := tmp.Size;

      if ofilesize = 0 then
      begin
        // Nothing to pack
        log.Add('WARNING: File is blank');
        checksum := 0;
      end
      else
      begin
        // Pack the file
        checksum := libMSF.msfChecksum(Byte(tmp.Memory^), tmp.Size); // hash        
        lzfile := PackLZMA(tmp); // compress
        tmp.Free;
        lzfile.Position:=0;
        libMSF.msfScramble(Byte(lzfile.Memory^), lzfile.Size); // scramble
      end;

      // Patch MRF (this must be done when filesize is 0 too)
      log.Add('Patching '+mrf+'..');

      // TODO: find a temporary name instead of deleting potential user data
      if fileexists(mrf+'_') then deletefile(mrf+'_');

      // Rename the existing MRF data
      RenameFile(mrf,mrf+'_');

      fs1 := TFileStream.Create(mrf, fmCreate);       // New MRF we create
      fs2 := TFileStream.Create(mrf+'_', fmOpenRead); // MRF we just renamed

      // Check if we need to copy any preceeding data
      if entry.entryData.offset > 0 then
      begin
        // Copy all the data before the offset
        fs1.CopyFrom( fs2, entry.entryData.offset );

        // Seek to the new position
        fs2.Seek( entry.entryData.offset, soBeginning ); 
      end;

      filesize := 0;

      // Check we can copy the new file to the MRF (not when size = 0)
      if ofilesize > 0 then
      begin
        lzfile.Position := 0;
        fs1.CopyFrom( lzfile, lzfile.Size );
        filesize := lzfile.Size;
        // size of new file - size of current
        deltaSize := lzfile.Size - entry.entryData.zsize;
        lzfile.Free;
      end
      else
      begin
        // zsize is now 0, so the difference is the original size
        deltaSize := -entry.entryData.zsize;
      end;

      // Copy any trailing data
      fs2.Seek( entry.entryData.offset + entry.entryData.zsize, soBeginning );

      if fs2.Position <> fs2.Size then
        fs1.CopyFrom( fs2, fs2.Size-fs2.Position );

      // Finally:
      fs1.Free;
      fs2.Free;

      // now delete fs2 (temporary file)
      if fileexists(mrf+'_') then deletefile(mrf+'_');

      entry.entryData.size    := ofilesize;
      entry.entryData.zsize   := filesize;
      entry.entryData.dataHash:= checksum;

      // Update offsets in database
      if deltaSize <> 0 then
      begin

        for j:=plist[i-1]+2 to msfi.FileCount() do
        begin
          entry2 := @(msfi.FileEntries[ j-1 ]);

          // update the offsets for files in the same mrf
          if ( entry2.mrfOwner = entry.mrfOwner )
           and ( entry2.mrfIndex = entry.mrfIndex ) then
          begin
            // bugfix: wrong sign here
            entry2.entryData.offset := entry2.entryData.offset+deltaSize;
          end;
        end;

      end;

      log.Add('File patched!');
      mystatus := i;
    end;

  end;

  // Write the MSF index
  log.Add('Exporting fileindex.msf..');

  msfi.SaveFileIndex(base+'fileindex.msf');
  log.Add('File index has been saved!');

  SetLength(plist,0);

  // Re-import file index

  log.Add('Reloading file index..');
  msfi.LoadFileIndex(base+'fileindex.msf');
  log.Add('File index re-imported!');

  // Mark the end of patching
  isOver := True;

end;

function MSFPatcher.finishedPatch(): Boolean;
begin
  Result := isOver;
end;

function MSFPatcher.getProgress: integer;
begin
  Result := myStatus;
end;

function MSFPatcher.getLogMessages(): TStringList;
begin
  assert( log <> nil );
  Result := log;
end;

end.

