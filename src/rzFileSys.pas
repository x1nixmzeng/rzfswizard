{
  rzfswizard
  x1nixmzeng (July 2012)
}
unit rzFileSys;

interface

uses
  Dialogs, ComCtrls, SysUtils, Classes, libMSF,
  ULZMADecoder, ULZMAEncoder{, ULZMACommon};

type
  // MSF file info structure
  FILEINDEX_ENTRY = packed record
    size,
    offset,
    zsize     : Cardinal; // uint32
    lenMRFN,
    lenName   : Word;     // uint16
    unknown   : Cardinal; // uint32 
  end;

  // File info in memory
  MSF_ENTRY = record
    mrfOwner,   // Owner index
    mrfIndex,   // Container index    
    fileIndex   // Name index 
             : Cardinal;
    entryData   // File info
             : FILEINDEX_ENTRY;

    replaceW : String;         
    // todo: store filename of replacement
  end;

  type MSF = class
  private

    MrfFiles,  // Array of unique MRF files
    FileList   // Array of EACH file in the file system
            : TStringList;

    FileEntries : array of MSF_ENTRY;
    Files       : Cardinal;

    function AddMrfFile( str : string ) : Cardinal;
    function GetMrfIndex( str : string) : Cardinal;
    procedure ImportIndex( FIndex: TMemoryStream );

  public
    constructor Create;
    destructor Destroy;

    function LoadFileIndex( FileName: String ) : Boolean;
    function FileCount() : Integer;

    function GetMrfList() : TStringList;

    function GetMrfPartCount(mrfIndex2: Cardinal): Cardinal;
    function GetFilesForMrf(mrfIndex: Cardinal): Cardinal;
    function GetTotalSizeForMrf(mrfIndex: Cardinal): Cardinal;
    function GetTotalZSizeForMrf(mrfIndex: Cardinal): Cardinal;

    procedure AddFilesToList( grid: TListItems; mrfIndex: cardinal );
    procedure ReplaceFile( mrfIndex, fileIndex: Cardinal; const fName: String );

    function GetFileReplacements(): TStringList;
  end;

  function UnLZMA(inFile: TMemoryStream; outSize:Cardinal): TMemoryStream;
  function PackLZMA(inFile: TMemoryStream): TMemoryStream;

implementation

function StripExt(From:String):String;
begin
  Result:= Copy(From, 0, Length(From)-Length(ExtractFileExt(From)));
end;

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

  str := Copy(str, 0, length(str)-length(Extractfileext(str)));

  for i := MrfFiles.Count downto 1 do
  begin
    if MrfFiles.Strings[i-1] = str then
    begin
      Result := i-1;
      Exit;
    end;
  end; 

  // else
  Result := MrfFiles.Add( str );

end;

function MSF.GetMrfIndex( str: string ) : Cardinal;
var
  i: integer;
begin

  Result := 0; // 0 is the index root
  str := ExtractFileExt(str);

  if ( length(Str) > 0 ) and ( str <> '.mrf' ) then
  begin

    i := 1;

    // atoi :
    while ( i < length(str) ) and ( str[i+1] in ['0'..'9'] ) do
    begin
      Result := 10 * Result + (ord(str[i+1])-48);
      inc(i);
    end;

  end;

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

    with FileEntries[Files] do
    begin
      FIndex.Read( entryData, SizeOf(FILEINDEX_ENTRY) );

      // Unpack strings:
      SetLength(mrfn, entryData.lenMRFN);
      FIndex.Read(pchar(mrfn)^, entryData.lenMRFN);

      SetLength(fn, entryData.lenName);
      FIndex.Read(pchar(fn)^, entryData.lenName);

      FileList.Add(fn);

      // Update remaining FileEntries properties
      fileIndex:= FileList.Count-1;
      
      mrfOwner := AddMrfFile(mrfn);
      mrfIndex := GetMrfIndex(mrfn);
      
      replaceW := '';

      Inc(Files);
    end;

  end;

  {$IFDEF DUMP_FILELIST}
    FileList.SaveToFile('filelist.txt');
  {$ENDIF}
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

function MSF.GetFilesForMrf(mrfIndex: Cardinal): Cardinal;
var i: Cardinal;
begin
  Result := 0;
  for i:=1 to Files do
    if FileEntries[i-1].mrfOwner = mrfIndex then
      inc(Result);
end;

function MSF.GetTotalSizeForMrf(mrfIndex: Cardinal): Cardinal; 
var i: Cardinal;
begin
  Result := 0;
  for i:=1 to Files do
    if FileEntries[i-1].mrfOwner = mrfIndex then
      inc( Result, FileEntries[i-1].entryData.size );
end;

function MSF.GetTotalZSizeForMrf(mrfIndex: Cardinal): Cardinal;
var i: Cardinal;
begin
  Result := 0;
  for i:=1 to Files do
    if FileEntries[i-1].mrfOwner = mrfIndex then
      inc( Result, FileEntries[i-1].entryData.zsize );
end;

{
  TODO: EXPAND THIS FUNCTION
}
procedure MSF.AddFilesToList( grid: TListItems; mrfIndex: cardinal );
var i: Cardinal;
begin
  for i:=1 to Files do
    if FileEntries[i-1].mrfOwner = mrfIndex then
    begin

      with grid.Add do
      begin

        Caption := FileList[i-1];

        // todo: formatting of this item
        
        SubItems.Add(IntToStr(integer(FileEntries[i-1].entryData.size)));

        if length(FileEntries[i-1].replaceW) > 0 then
          SubItems.Add( FileEntries[i-1].replaceW )
        else
          SubItems.Add('<none>');

      end;

    end;
end;

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

        // do the file replacement here

        FileEntries[i-1].replaceW := fName;
        Exit;

      end;

      inc(fc);

    end;

end;

function MSF.GetFileReplacements(): TStringList;
var i: integer;
begin
  Result := TStringList.create;

  for i:=1 to Files do
    if FileEntries[i-1].replaceW <> '' then
      Result.Append( FileEntries[i-1].replaceW );

  // free'd after call
end;

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


end.