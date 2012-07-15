{
  rzfswizard
  x1nixmzeng (July 2012)

  Initial UI design 
}
unit UI;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus, libMSF, rzFileSys, XPMan;

type

  TPatcher = class(TThread)
    public
        fpacked: TMemoryStream;
    private
      myStatus: integer;
    protected
      procedure Execute; override;
    public
      function getProgress: integer;
  end;

  TWizUI = class(TForm)
    pnHeader: TPanel;
    pnFooter: TPanel;
    btnNext: TButton;
    btnBack: TButton;
    lbInfo: TLabel;
    tcMain: TPageControl;
    tsBrowse: TTabSheet;
    tsTarget: TTabSheet;
    edPath: TEdit;
    lblHead1: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ListView1: TListView;
    tsSelect: TTabSheet;
    Label7: TLabel;
    ListView2: TListView;
    Label8: TLabel;
    Button4: TButton;
    tsProgress: TTabSheet;
    pbPatch: TProgressBar;
    btnBrowse: TButton;
    OpenDialog1: TOpenDialog;
    imHead: TImage;
    lblInfo: TLabel;
    lblBuild: TLabel;
    pmExtract: TPopupMenu;
    Extract1: TMenuItem;
    N1: TMenuItem;
    ExtractAll1: TMenuItem;
    lblInfo1: TLabel;
    lblStatusTxt: TLabel;
    lblStatus: TLabel;
    pmNull: TPopupMenu;
    btnReset: TButton;
    Label24: TLabel;
    Button2: TButton;
    Button1: TButton;
    Label2: TLabel;
    mPatch: TMemo;
    XPManifest1: TXPManifest;
    Label10: TLabel;
    Edit1: TEdit;
    Reset: TButton;
    Button5: TButton;
    Label12: TLabel;
    Label13: TLabel;
    CheckBox1: TCheckBox;
    Panel1: TPanel;
    Button3: TButton;
    Label1: TLabel;
    Label3: TLabel;
    PopupMenu1: TPopupMenu;
    SaveLog1: TMenuItem;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    Label4: TLabel;
    Button6: TButton;
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure pmExtractPopup(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    function ProcessNavNext(tab: integer) : Boolean;
    function ProcessNavPrev(tab: integer) : Boolean;

    procedure ShowMessage(str: String);

    procedure ResetGlobalFileIndex() ;

    procedure PrePatchChecks();

    function PackFile(fname: String; var osize: integer) : TMemoryStream;

  public
    { Public declarations }
  end;

var
  WizUI: TWizUI;
  FileIndex : rzFileSys.MSF;

  dispAsBytes : bool = false;

implementation

{$R *.dfm}

//
// Generic 'hidden-tab navigation'
//

procedure TWizUI.btnNextClick(Sender: TObject);
begin

  // Verify the next page can be navigated to

  if WizUI.tcMain.ActivePageIndex +1 < WizUI.tcMain.PageCount then
  begin

    // Check the required info on the current tab is completed

    if ProcessNavNext(WizUI.tcMain.ActivePageIndex +1) then
    begin
      // Enable back button
      WizUI.btnBack.Enabled := True;

      // Show next tab
      WizUI.tcMain.Pages[WizUI.tcMain.ActivePageIndex +1].Show;

      // Disable next button if last tab
      if WizUI.tcMain.ActivePageIndex = WizUI.tcMain.PageCount -1 then
        WizUI.btnNext.Enabled := False;
    end;

  end;

end;

procedure TWizUI.btnBackClick(Sender: TObject);
begin

  // Verify the previous page can be navigated to

  if WizUI.tcMain.ActivePageIndex -1 >= 0 then
  begin

    if ProcessNavPrev(WizUI.tcMain.ActivePageIndex -1) then
    begin

      WizUI.btnNext.Enabled := True;
      WizUI.tcMain.Pages[WizUI.tcMain.ActivePageIndex -1].Show;

      if WizUI.tcMain.ActivePageIndex = 0 then
        WizUI.btnBack.Enabled := False;
    end;
  end;

end;

//
// Few helper functions
//

procedure TWizUI.ResetGlobalFileIndex() ;
begin

  if FileIndex <> nil then
  begin

    // Clear any existing memory
    FileIndex.Destroy();
    FileIndex := nil;

  end;

  edPath.Text := '';

  lblStatus.Caption:='No file selected';
  lblStatus.Font.Color := clMaroon;
  
end;

function GetCurPath() : String;
begin

  Result := GetCurrentDir();

  // Prepend directory with backslash
  
  if Result[ strlen(@Result) ] <> '\' then
    Result := Result + '\';

end;

function IntToStr2( const int: Cardinal ): String;
var
  str : String;
  i : integer;
begin
  str := IntToStr( int );
  i := length(str);
  
  Result := '';

  while i > 3 do
  begin
    Result := ',' + copy(str, i-2, 3) + Result;
    dec(i,3);
  end;

  Result := {Format('%20s',[}copy(str, 0, i) + Result{])};

end;

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

//
//
//

procedure TWizUI.pmExtractPopup(Sender: TObject);
begin
  pmExtract.Items.Items[0].Enabled := (ListView1.SelCount <> 0);
end;

procedure TWizUI.btnBrowseClick(Sender: TObject);
var
  sl : TStringList;  i : cardinal;
begin
  if OpenDialog1.Execute then
  begin

    edPath.Text := OpenDialog1.Files[0];

    if FileIndex <> nil then
      FileIndex.Destroy;

    FileIndex := rzFileSys.MSF.Create;

    if FileIndex.LoadFileIndex(edPath.text) then
    begin

      sl := TStringList.Create;
      sl := FileIndex.GetMrfList();

      // Remove previous MRF list
      ListView1.Items.Clear;

      // Add all MRF items
      for i := 1 to sl.Count do
      begin

        with ListView1.Items.Add do
        begin
          {
           NAME
           PARTS
           SIZE
           ZSIZE
           FILES
          }

          caption := sl.Strings[i-1];
          SubItems.Add( IntToStr2( FileIndex.GetMrfPartCount(i-1) ) );
          SubItems.Add( IntToSize( FileIndex.GetTotalSizeForMrf(i-1) ) );
          SubItems.Add( IntToSize( FileIndex.GetTotalZSizeForMrf(i-1) ) );
          SubItems.Add( IntToStr2( FileIndex.GetFilesForMrf(i-1) ) );
        end;

      end;

      if sl.Count > 0 then
        ListView1.Items[0].Selected := true;

      lblStatus.Caption := Format('Success! Located %d files', [FileIndex.FileCount()]);
      lblStatus.Font.Color := clGreen;

    end;

  end;
end;

procedure TWizUI.FormCreate(Sender: TObject);
begin
  // Correct the entry tab
  if tcMain.TabIndex <> 0 then tcMain.Pages[0].Show;

  btnBack.Enabled := False;

  msfMakeHashTable();
  ResetGlobalFileIndex();

end;

function TWizUI.ProcessNavNext(tab: integer) : Boolean;
begin
  {
    gatemaster naviation
    checks the requirements for each tab before progressing
  }

  Result := False;

  case tab of
    0: begin
         assert( false )
       end;
    1: begin
         if WizUI.edPath.Text <> '' then
         begin

           // move on
           Result := True;

         end
         else
           ShowMessage('No MSF file is selected.'#13#10'Select one to continue!');
       end;
    2 : begin
          if ListView1.SelCount = 1 then
          begin

            ListView2.Items.Clear;
            FileIndex.AddFilesToList( ListView2.Items, ListView1.Selected.Index );

            if ListView2.Items.Count > 0 then
              ListView2.Items.Item[0].Selected := True;


            Result := True;
          end
          else
            ShowMessage('Select a subfolder to continue');
        end;
    3 : begin

          PrePatchChecks();
          Result := True;

        end;
  end;

end;

function TWizUI.ProcessNavPrev(tab: integer) : Boolean;
begin
  Result := False;

  case tab of
    0: begin

        // btnBack.Enabled := false;
         Result := True;

       end;

    1: begin
        (*
        if MessageBoxA(
          Handle,
          'You can only modify one MRF at a time' +
          #13#10 +
          'Do you want to lose changes?',
          'Confirm',
          MB_ICONASTERISK or MB_YESNO{CANCEL}
        ) = mrYes then*)
        begin
          Result := True;
        end;
      end;
    2: begin

         //ShowMessage('Patch is in progress!');
         Result := True;

       end;
  end;
end;

procedure TWizUI.ShowMessage(str: String);
begin
  MessageBoxA( Handle, PAnsiChar(str), 'Information', MB_OK or MB_ICONASTERISK );
end;

procedure TWizUI.btnResetClick(Sender: TObject);
begin
  ResetGlobalFileIndex();
end;

procedure TWizUI.FormDestroy(Sender: TObject);
begin
  msfClearHashTable();

  if FileIndex <> nil then
    FileIndex.Destroy;
end;

procedure TWizUI.Button4Click(Sender: TObject);
var
  newFile : TOpenDialog;
begin
  if ListView2.SelCount = 1 then
  begin

    newFile := TOpenDialog.Create( WizUI );

    newFile.Filter := 'Any File (*.*)|*.*';
    newFile.FilterIndex := 0;
    newFile.Title := 'Replace With..';

    // Ask user for file
    if newFile.Execute then
    begin

      // Re-check the file exists
      if FileExists( newFile.Files[0] ) then
      begin
        // replace in fs
        FileIndex.ReplaceFile(
          ListView1.Selected.Index,
          ListView2.Selected.Index,
          newFile.Files[0] );

        // replace (temporarily) in filelist 
        ListView2.Selected.SubItems.Strings[1] := newFile.Files[0];
      end;

    end;

    newFile.Destroy;

  end;
end;

procedure TWizUI.Button1Click(Sender: TObject);
var
  i: integer;
  ms, tmp: tmemorystream;
  d, item : string;
begin

  d := GetCurPath();

  for i := 1 to ListView2.Items.Count do
  begin

    item := ListView2.Items.Item[i-1].SubItems[1];

    if ( item <> '<none>' ) and ( FileExists( item ) ) then
    begin

      ms := tmemorystream.Create;

      try
        ms.LoadFromFile( item );

        tmp := PackLZMA( ms );
        libMSF.msfScramble( Byte(tmp.Memory^), tmp.Size );

        // todo: make filename
        item := d + Format('mrf_%d.tmp',[i-1]);
        tmp.SaveToFile( item );

        tmp.Free;

      finally
        ms.Free;
      end;

    end;


  end;

end;

{
procedure TWizUI.RunPatcher();
var
  i: integer;
  sl : TStringList;

  procedure Log( str: String) ;
  begin
    mPatch.Lines.Add( Format('%s  %s', [TimeToStr(Now),str] ) );
  end;

begin

  pbPatch.Min := 0;
  pbPatch.Position := 0;
  pbPatch.Max := 100; // PRELIM MAX. max will increase with filesizes?

  mPatch.Lines.Clear();
  Log('Patch started');

  // todo: create thread which check files

  sl := FileIndex.GetFileReplacements();

  Log('Locating replacement files..');

  for i:=0 to sl.Count-1 do
  begin

    if not FileExists(sl.Strings[i]) then
    begin
      Log('Failed to open "' + sl.strings[i] + '"');
    end;

  end;

  Log( 'Found ' + IntToStr(sl.Count) + ' marked changes' );  

  sl.Free;


end;
      }

procedure TWizUI.PrePatchChecks();
begin

  if FileIndex = nil then Exit;

  // list the number of file replacements
  label3.Caption := IntToStr( FileIndex.CountFileReplacements() );

end;

procedure TWizUI.CheckBox1Click(Sender: TObject);
begin
  Panel1.Visible:= TCheckBox(Sender).Checked;
end;

{
   compress file
}
function TWizUI.PackFile(fname: String; var osize: integer) : TMemoryStream;
var
  inFile : TMemoryStream;
begin
  osize := -1; // invalid size
  if not fileexists(fname) then begin
    result:=nil;
    exit;
  end;

  inFile:=TMemoryStream.Create;

  // lzma compressiong (TODO: PROGRESS METER)
  Result:=rzFileSys.PackLZMA(inFile);

  // msf scrambling
  libMSF.msfScramble( Byte(Result.Memory^), Result.Size );
  
  inFile.Free;

end;

procedure TWizUI.Button3Click(Sender: TObject);
var
  rcnt : Cardinal;
  patchth: TPatcher;

  procedure Log( str: String) ;
  begin
    mPatch.Lines.Add( Format('%s  %s', [TimeToStr(Now),str] ) );
  end;  

begin

  TButton(Sender).Enabled := False;
  pbPatch.Min := 0;
  pbPatch.Position := 0;
  pbPatch.Max := 100; // preliminary position (could be updated for patch size)
   
  Application.ProcessMessages;

  // This should NOT be the case at this tab (unless built like it)
  if FileIndex = nil then
  begin
    TButton(Sender).Enabled := True;
    Exit;
  end;

  rcnt := FileIndex.CountFileReplacements();

  if rcnt = 0 then
  begin
  (**************************************
    You have not marked any changes.
    There is nothing to patch!
  ***************************************)
    ShowMessage('You have not marked any changes.'#13#10'There is nothing to patch!');
    TButton(Sender).Enabled := True;
    Exit;
  end;

  mPatch.Clear;
  Log('Patching starts');
  if rcnt = 1 then Log('1 file has been marked')
  else             Log(IntToStr(rcnt)+' files have been marked');


  {

  // todo: push some data here and begin the patching process
  patchth := TPatcher.Create( true );
  patchth.FreeOnTerminate := False;
  patchth.Resume;

  while patchth.getProgress() < 100 do
  begin
    // update progress bar
    pbPatch.Position := patchth.getProgress();
    application.ProcessMessages;
  end;

  patchth.Free;

  }

  Log('Patching finished');

  TButton(Sender).Enabled := True;

end;

procedure TWizUI.Timer1Timer(Sender: TObject);
begin
  if pbPatch.Position = pbPatch.Max then timer1.Enabled:=False
  else
    pbPatch.Position := pbPatch.Position +1;
end;
        {
constructor TPatcher.Create;
begin
  inherited Create;
  // setup some private members here
  myStatus := 0;
  FreeOnTerminate := True;
end;
       }
procedure TPatcher.Execute;
begin

  while myStatus < 101 do
  begin
    inc(myStatus);
    //Sleep(100);
  end;

end;

function TPatcher.getProgress: integer;
begin
  Result := myStatus;
end;

procedure TWizUI.Button6Click(Sender: TObject);
begin
  if FileIndex = nil then exit;

  FileIndex.SaveFileIndex('lolwhat.msf');

end;

end.
