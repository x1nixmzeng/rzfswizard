{
  rzfswizard
  x1nixmzeng (July 2012)
}

program rzfswizard;

uses
  Forms,
  UI in 'UI.pas',
  libMSF in 'libMSF.pas',
  URangeEncoder in 'LZMA\URangeEncoder.pas',
  UBitTreeDecoder in 'LZMA\UBitTreeDecoder.pas',
  UBitTreeEncoder in 'LZMA\UBitTreeEncoder.pas',
  ULZBinTree in 'LZMA\ULZBinTree.pas',
  ULZInWindow in 'LZMA\ULZInWindow.pas',
  ULZMABase in 'LZMA\ULZMABase.pas',
  ULZMACommon in 'LZMA\ULZMACommon.pas',
  ULZMADecoder in 'LZMA\ULZMADecoder.pas',
  ULZMAEncoder in 'LZMA\ULZMAEncoder.pas',
  ULZOutWindow in 'LZMA\ULZOutWindow.pas',
  URangeDecoder in 'LZMA\URangeDecoder.pas',
  rzFileSys in 'rzFileSys.pas';

{$R *.res}

begin
  Application.Initialize;

  Application.Title := 'RaiderZ FileSystem Wizard';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
