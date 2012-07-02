{
  rzfswizard
  x1nixmzeng (July 2012)

  Initial UI design 
}
unit UI;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus, libMSF, rzFileSys;

type
  TForm1 = class(TForm)
    pnHeader: TPanel;
    pnFooter: TPanel;
    btnNext: TButton;
    btnBack: TButton;
    lbInfo: TLabel;
    tcMain: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    edPath: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ListView1: TListView;
    TabSheet4: TTabSheet;
    Label7: TLabel;
    TabSheet5: TTabSheet;
    Label10: TLabel;
    Label11: TLabel;
    ListView2: TListView;
    Label8: TLabel;
    Label12: TLabel;
    ListBox1: TListBox;
    Button4: TButton;
    Label14: TLabel;
    TabSheet6: TTabSheet;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Button5: TButton;
    btnBrowse: TButton;
    OpenDialog1: TOpenDialog;
    Image1: TImage;
    Label4: TLabel;
    Label9: TLabel;
    PopupMenu1: TPopupMenu;
    Extract1: TMenuItem;
    N1: TMenuItem;
    ExtractAll1: TMenuItem;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    PopupMenu2: TPopupMenu;
    Button1: TButton;
    Label24: TLabel;
    Button2: TButton;
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
  private
    function ProcessNavNext(tab: integer) : Boolean;
    function ProcessNavPrev(tab: integer) : Boolean;

    procedure ShowMessage(str: String);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  FileIndex : rzFileSys.MSF;

implementation

{$R *.dfm}

procedure TForm1.btnNextClick(Sender: TObject);
begin

  // Check there is a next page (should always be true)

  if Form1.tcMain.ActivePageIndex +1 < Form1.tcMain.PageCount then
  begin

    // Check the required info on the current tab is completed

    if ProcessNavNext(Form1.tcMain.ActivePageIndex +1) then
    begin
      // Enable back button
      Form1.btnBack.Enabled := True;

      // Show next tab
      Form1.tcMain.Pages[Form1.tcMain.ActivePageIndex +1].Show;

      // Disable next button if last tab
      if Form1.tcMain.ActivePageIndex = Form1.tcMain.PageCount -1 then
        Form1.btnNext.Enabled := False;
    end;

  end;

end;

procedure TForm1.btnBackClick(Sender: TObject);
begin



    if Form1.tcMain.ActivePageIndex -1 >= 0 then
    begin
      Form1.btnNext.Enabled := True;
      Form1.tcMain.Pages[Form1.tcMain.ActivePageIndex -1].Show;

      if Form1.tcMain.ActivePageIndex = 0 then
        Form1.btnBack.Enabled := False;

    end;


end;

function GetFileExtLen( const FileName: String ): Integer;
begin
  Result := Length(ExtractFileExt( FileName )) ;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
  popupmenu1.Items.Items[0].Enabled := (ListView1.SelCount <> 0);
//  popupmenu1.Items.Items[2].Enabled := (ListView1.SelCount <> 0);
end;

procedure TForm1.btnBrowseClick(Sender: TObject);
var
  sl : TStringList;  i : cardinal;
begin
  if OpenDialog1.Execute then
  begin

    edPath.Text := OpenDialog1.Files[0];

    FileIndex.Destroy;
    FileIndex := rzFileSys.MSF.Create;

    if FileIndex.LoadFileIndex(edPath.text) then
    begin

//    rzFileSys.

      sl := TStringList.Create;
      sl := FileIndex.GetMrfList();

      for i := 1 to sl.Count do
      begin
        with ListView1.Items.Add do
        begin
          caption := sl.Strings[i-1];
          // File count
          SubItems.Add( IntToStr( FileIndex.GetFilesForMrf(i-1) ) );
          // Packed Size
          SubItems.Add('0');
          // Total Size
          SubItems.Add('0');
        end;

      end;

      label21.Caption := Format('Loaded %d files', [FileIndex.FileCount()]);
      label21.Font.Color := clGreen;

    end;

//    label21.Caption := 'Unsupported MSF format';
//    label21.Font.Color := clMaroon;

  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if tcMain.TabIndex <> 0 then
    tcMain.Pages[0].Show;

  // Disable navigation buttons
  btnBack.Enabled := False;
  label21.Caption := 'No file selected';
  label21.Font.Color := clMaroon;

  msfMakeHashTable();
  FileIndex := rzFileSys.MSF.Create;

end;

function TForm1.ProcessNavNext(tab: integer) : Boolean;
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
         if Form1.edPath.Text <> '' then
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

            Result := True;
          end
          else
            ShowMessage('Select a subfolder to continue');
        end;
  end;

end;

function TForm1.ProcessNavPrev(tab: integer) : Boolean;
begin
  Result := False;
end;

procedure TForm1.ShowMessage(str: String); 
begin
  MessageBoxA( Handle, PAnsiChar(str), 'Information', MB_OK or MB_ICONASTERISK );
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  edPath.Text := '';
  label21.Caption:='No file selected';
  label21.Font.Color := clMaroon;  
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  msfClearHashTable();

  FileIndex.Destroy;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if ListView2.SelCount = 1 then
  begin

  // replace file

    ListView2.Selected.SubItems.Strings[1] := 'C:\test.bmp';

  end;
end;

procedure TForm1.ListView1ColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if Column.Index = 1 then
  begin
    ListView1.Items.Item[0].SubItems[0] := 'test';
  end;
end;

end.
