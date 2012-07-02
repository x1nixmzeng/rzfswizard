{
  rzfswizard
  x1nixmzeng (July 2012)
}
unit rzFileSys;

interface

uses Dialogs, ComCtrls, SysUtils, Classes, libMSF, ULZMADecoder;

// ULZMAEncoder;

//  function PackLZMA(inFile: TMemoryStream): TMemoryStream;

type
  FILEINDEX_ENTRY = packed record
    size,
    offset,
    zsize     : Cardinal; // uint32
    
    lenMRFN,
    lenName   : Word;     // uint16

    unknown   : Cardinal; // uint32 
  end;

  MSF_ENTRY = packed record
    mrfOwner,   // Index to MRF name
    fileIndex,  // Index to string
    mrfOffset,  // MRF file position offset
    pkSize,     // Packed file size (stored in MRF)
    unSize      // Unpacked file size
             : Cardinal;
    lstMod      // Last recorded modified time
             : Integer;
  end;

  type MSF = class
  private

    MrfFiles,  // Array of unique MRF files
    FileList   // Array of EACH file in the file system
            : TStringList;

    FileEntries : array of MSF_ENTRY;
    Files       : Cardinal;

    function AddMrfFile( str : string ) : Cardinal;
    procedure InsertEntry( var name: String;
                           var mrfName: String;
                           offset, size, usize: Cardinal );
    procedure ImportIndex( FIndex: TMemoryStream );

  public
    constructor Create;
    destructor Destroy;

    function LoadFileIndex( FileName: String ) : Boolean;
    function FileCount() : Integer;

    function GetMrfList() : TStringList;
    function GetFilesForMrf(mrfIndex: Cardinal): Cardinal;

    procedure AddFilesToList( grid: TListItems; mrfIndex: cardinal );

  end;

  function UnLZMA(inFile: TMemoryStream; outSize:Cardinal): TMemoryStream;

implementation

function UnLZMA(inFile: TMemoryStream; outSize:Cardinal): TMemoryStream;
const
  LZMA_PROPS_SIZE = 5;
  LZMA_HEADER_EX : array[0..LZMA_PROPS_SIZE-1] of byte =
  ( $5D, $00, $00, $01, $00 );

var
  decoder:TLZMADecoder;
begin

  Result := TMemoryStream.Create;

  decoder := TLZMADecoder.Create;

  try
    decoder.SetDictionarySize(0);
    decoder.SetDecoderProperties(LZMA_HEADER_EX);
    decoder.Code(inFile, Result, outSize);
  finally
    decoder.Free;
  end;

end;

constructor MSF.Create;
begin
  inherited Create;

  MrfFiles := TStringList.Create;
  FileList := TStringList.Create;

  if not libMSF.msfGetHashTable() then
  begin
    libMSF.msfMakeHashTable();
  end;

  SetLength(FileEntries, 0);
  Files    := 0;
end;

destructor MSF.Destroy;
begin
  MrfFiles.Free;
  FileList.Free;

  SetLength(FileEntries, 0);
  Files    := 0;

  inherited Destroy;
end;

function MSF.AddMrfFile( str : string ) : Cardinal;
var i: integer;
begin

  // Check the file doesn't already exist (most cases)

  for i := 1 to MrfFiles.Count do
  begin
    if MrfFiles.Strings[i-1] = str then
    begin
      Result := i-1;
      Exit;
    end;
  end;

  // Add the file

  MrfFiles.Add( str );
  Result := MrfFiles.Count-1;

end;

procedure MSF.InsertEntry( var name: String;
                           var mrfName: String;
                           offset, size, usize: Cardinal );
begin

  // Reallocate memory for new structure

  inc(Files);
  SetLength(FileEntries, Files);
        {
  FileList.Add( name );
  
  FileEntries[Files-1].fileIndex := FileList.Count -1;
  FileEntries[Files-1].mrfOwner  := AddMrfFile( mrfName );
  FileEntries[Files-1].mrfOffset := offset;
  FileEntries[Files-1].pkSize    := size;
  FileEntries[Files-1].unSize    := usize;
             }
end;

procedure MSF.ImportIndex( FIndex: TMemoryStream );
var
  fRec : FILEINDEX_ENTRY;
  fn, mrfn: string;
begin

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

  SetLength(FileEntries, Files);

  FIndex.Position := 0;
  Files := 0;
  
  // Then loop through reading each entry
  //
  while (FIndex.Position < FIndex.Size) do
  begin

    //
    FIndex.Read( fRec, SizeOf(FILEINDEX_ENTRY) );


    SetLength(mrfn, fRec.lenMRFN);
    FIndex.Read(pchar(mrfn)^, fRec.lenMRFN);
//    mrfn[fRec.mrfLen] := #0;

    SetLength(fn, fRec.lenName);
    FIndex.Read(pchar(fn)^, fRec.lenName);
//    fn[fRec.fnLen+1] := #0;

    //InsertEntry(fn, mrfn, fRec.mrfOff, fRec.size, fRec.uSize);
    FileList.Add(fn);

    FileEntries[Files].fileIndex:= FileList.Count-1;
    FileEntries[Files].mrfOwner := AddMrfFile(mrfn);
    FileEntries[Files].mrfOffset:= fRec.offset;
    FileEntries[Files].pkSize   := fRec.zsize;
    FileEntries[Files].unSize   := fRec.size;

    Inc(Files);

  end;

  FileList.SaveToFile('filelist.txt');

end;

function MSF.LoadFileIndex( FileName: String ) : Boolean;
var
  FIndex   : TFileStream;
  unSize   : Cardinal;

  UnpackBuf,
  tmpBuffer: TMemoryStream;
begin

  Result := False;

  if not FileExists( FileName ) then Exit;

  // Open the file for reading

  FIndex := TFileStream.Create( FileName, fmOpenRead );

  // Get the uncompressed size and copy data to buffer

  FIndex.Read(unSize, 4);

  UnpackBuf := TMemoryStream.Create;
  UnpackBuf.CopyFrom(FIndex, FIndex.Size - 4);
  FIndex.Free;

  // Unscramble the data
  UnpackBuf.Position := 0;
  libMSF.msfUnscramble(Byte(UnpackBuf.Memory^), UnpackBuf.Size);

  // Unpack in memory
  tmpBuffer := UnLZMA(UnpackBuf, unSize);
  UnpackBuf.Free;

  // Import the file data
  tmpBuffer.Position := 0;
  ImportIndex( tmpBuffer );
  
  tmpBuffer.Free;
  Result := True;

end;

function MSF.FileCount : Integer;
begin
  Result := FileList.Count;
end;

function MSF.GetMrfList() : TStringList;
begin
  Result := MrfFiles;
end;

function MSF.GetFilesForMrf(mrfIndex: Cardinal): Cardinal; 
var i: Cardinal;
begin
  Result := 0;
  for i:=1 to Files do
    if FileEntries[i-1].mrfOwner = mrfIndex then
      inc(Result);
end;

procedure MSF.AddFilesToList( grid: TListItems; mrfIndex: cardinal );
var i: Cardinal;
begin
  for i:=1 to Files do
    if FileEntries[i-1].mrfOwner = mrfIndex then
    begin

      with grid.Add do
      begin

        Caption := FileList[i-1];
        SubItems.Add(IntToStr(integer(FileEntries[i].unSize)));
        //SubItems.Add(IntToStr(integer(FileEntries[i].pkSize)));

        {
          Not extracted
          Last modified
          Modified
        }

        SubItems.Add('<none>');


      end;

    end;
end;


  

 // FIndex.sa

//  tmp :=GetCurrentDir();

{  for i:=0 to MrfFiles.Count-1 do
  begin

    for j := 0 to length(mrffiles.Strings[i])-1 do
      if pchar(mrffiles.Strings[i])[j] = '/' then
        pchar(mrffiles.Strings[i])[j] := '\';

    if not fileexists(CurrentPath() + mrffiles.Strings[i]) then
    begin

    // issue: zero ending string
      doLog( 'File not found "'+(CurrentPath()+mrfFiles.Strings[i])+'"' );

    end;
  end;     }

//  form1.label1.Caption := inttostr(reccount)+' files here';




{

var
  x :tmemorystream;
begin
  {x:=tmemorystream.Create;
  x.LoadFromFile('diditwork.dat');

  with PackLZMA(x) do
  begin
    SaveToFile('test.dat');
    Free;
  end;

  x.Free;  
       
function PackLZMA(inFile: TMemoryStream): TMemoryStream;
var
  encoder:TLZMAEncoder;
begin
  encoder := TLZMAEncoder.Create;

//  encoder.SetAlgorithm(2);  does NOTHING.. check the source
  encoder.SetDictionarySize($10000);
//  encoder.SeNumFastBytes(128) ; ??
//  encoder.SetMatchFinder(1) ;   ??   
  encoder.SetLcLpPb(3,0,2);     // defaults?!



  Result := TMemoryStream.Create;

  inFile.Position :=0;
  encoder.Code(inFile, Result, inFile.Size, -1);

  encoder.Free;
end;

  }


end.