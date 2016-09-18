unit ufEmutecaMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls,
  Graphics, Dialogs,
  ActnList, Menus, StdActns, ComCtrls, ExtCtrls, DefaultTranslator,
  IniPropStorage, StdCtrls,
  // Misc
  uVersionSupport,
  // CHX units
  uCHXStrUtils,
  // CHX forms
  ufCHXAbout, ufCHXProgressBar,
  // CHX frames
  ufTagTree,
  // Emuteca common
  uEmutecaCommon,
  // Emuteca clases
  ucEmuteca, ucEmutecaParent, ucEmutecaSoftware, ucEmutecaSystem,
  // Emuteca forms
  ufEmutecaScriptManager,
  // Emuteca frames
  ufEmutecaParentList, ufEmutecaSoftList, ufEmutecaEmulatorManager,
  ufEmutecaSystemManager, ufEmutecaSystemCBX,
  // Emuteca windows
  ufEmutecaActAddSoft, ufEmutecaActAddFolder,
  uGUIConfig;

type

  { TfrmEmutecaMain }

  TfrmEmutecaMain = class(TForm)
    actEmulatorManager: TAction;
    actAddFolder: TAction;
    actAddSoft: TAction;
    actSaveLists: TAction;
    actMediaManager: TAction;
    actScriptManager: TAction;
    actSystemManager: TAction;
    ActionList1: TActionList;
    FileExit1: TFileExit;
    HelpOnHelp1: THelpOnHelp;
    ImageList1: TImageList;
    IniPropStorage1: TIniPropStorage;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mmHelp: TMenuItem;
    mmFile: TMenuItem;
    MenuItem2: TMenuItem;
    PageControl1: TPageControl;
    pBottom: TPanel;
    pMiddle: TPanel;
    pRight: TPanel;
    pSystems: TPanel;
    pTop: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    stbHelp: TStatusBar;
    stbInfo: TStatusBar;
    procedure actAddSoftExecute(Sender: TObject);
    procedure actAddFolderExecute(Sender: TObject);
    procedure actEmulatorManagerExecute(Sender: TObject);
    procedure actSaveListsExecute(Sender: TObject);
    procedure actScriptManagerExecute(Sender: TObject);
    procedure actSystemManagerExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpOnHelp1Execute(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);

  private
    { private declarations }
    FEmuteca: cEmuteca;
    FGUIConfig: cGUIConfig;

    // Frames
    fmEmutecaSystemCBX: TfmEmutecaSystemCBX;
    fmEmutecaParentList: TfmEmutecaParentList;
    fmEmutecaSoftList: TfmEmutecaSoftList;
    fmCHXTagTree: TfmTagTree;
    procedure SetGUIConfig(AValue: cGUIConfig);

  protected
    property Emuteca: cEmuteca read FEmuteca;
    property GUIConfig: cGUIConfig read FGUIConfig write SetGUIConfig;

    procedure CheckTags(aList: TStrings);
    procedure SelectParent(const aParent: cEmutecaParent);
    procedure SelectSoftware(const aSoftware: cEmutecaSoftware);
    function SelectSystem(aSystem: cEmutecaSystem): boolean;

    procedure RunVersion(const aSoftware: cEmutecaSoftware);

    procedure SaveEmuteca;

    function OnProgressBar(const Title, Info1, Info2: string;
      const Value, MaxValue: int64): boolean;

  public
    { public declarations }
  end;

var
  frmEmutecaMain: TfrmEmutecaMain;

implementation

{$R *.lfm}

{ TfrmEmutecaMain }

procedure TfrmEmutecaMain.HelpOnHelp1Execute(Sender: TObject);
begin
  Application.CreateForm(TfrmCHXAbout, frmCHXAbout);
  frmCHXAbout.ShowModal;
  FreeAndNil(frmCHXAbout);
end;

procedure TfrmEmutecaMain.MenuItem8Click(Sender: TObject);
var
  Temp: TStringList;
  str: string;
  i: integer;
begin
  Temp := TStringList.Create;

  for i := 1 to 10000 do
  begin
    str := IntToStr(i) + ',';
    str := str + str + str + str + str + str + str;
    Temp.Add(str);
  end;

  Temp.SaveToFile('temp.csv');
  FreeAndNil(Temp);
end;

procedure TfrmEmutecaMain.SetGUIConfig(AValue: cGUIConfig);
begin
  if FGUIConfig = AValue then
    Exit;
  FGUIConfig := AValue;
end;

procedure TfrmEmutecaMain.CheckTags(aList: TStrings);
begin
  { TODO : Pasar la lista para filtrar los padres y los juegos }
end;

procedure TfrmEmutecaMain.SelectParent(const aParent: cEmutecaParent);
begin
  Emuteca.CurrentParent := aParent;
  // Unselecting current software
  SelectSoftware(nil);
end;

procedure TfrmEmutecaMain.SelectSoftware(const aSoftware: cEmutecaSoftware);
begin
  Emuteca.CurrentSoft := aSoftware;
end;

function TfrmEmutecaMain.SelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  Result := True;
  Emuteca.CurrentSystem := aSystem;

    { TODO : Use Observer pattern... }
    fmEmutecaSoftList.UpdateList;
    fmEmutecaParentList.UpdateList;
end;

procedure TfrmEmutecaMain.RunVersion(const aSoftware: cEmutecaSoftware);
begin
  Emuteca.RunSoftware(aSoftware);
end;

procedure TfrmEmutecaMain.SaveEmuteca;
begin
  { TODO : Emuteca.Save }
  Emuteca.ParentManager.SaveToFile('', False);
  Emuteca.SoftManager.SaveToFile('', False);
end;

function TfrmEmutecaMain.OnProgressBar(const Title, Info1, Info2: string;
  const Value, MaxValue: int64): boolean;
begin
  // Really we can asume that frmCHXProgressBar is allways created...;
  if not Assigned(frmCHXProgressBar) then
    Application.CreateForm(TfrmCHXProgressBar, frmCHXProgressBar);
  Result := frmCHXProgressBar.UpdTextAndBar(Title, Info1, Info2,
    Value, MaxValue);
end;

procedure TfrmEmutecaMain.FormCreate(Sender: TObject);

  procedure CreateFrames;
  var
    aTabSheet: TTabSheet;
  begin
    // Better create frames in code while developing...
    //  IDE has many problems updating inherited properties

    // Creating and Setting the System ComboBox
    fmEmutecaSystemCBX := TfmEmutecaSystemCBX.Create(pMiddle);
    fmEmutecaSystemCBX.Parent := pMiddle;
    fmEmutecaSystemCBX.Align := alTop;
    fmEmutecaSystemCBX.OnSelectSystem := @Self.SelectSystem;
    fmEmutecaSystemCBX.SystemList := Emuteca.SystemManager.VisibleList;

    // Creating and setting the parent list frame
    fmEmutecaParentList := TfmEmutecaParentList.Create(pTop);
    fmEmutecaParentList.Parent := pTop;
    fmEmutecaParentList.OnItemSelect := @Self.SelectParent;
    fmEmutecaParentList.ParentList := Emuteca.ParentManager.FullList;

    // Creating and Setting the software list frame
    fmEmutecaSoftList := TfmEmutecaSoftList.Create(pBottom);
    fmEmutecaSoftList.Parent := pBottom;
    fmEmutecaSoftList.OnItemSelect := @Self.SelectSoftware;
    fmEmutecaSoftList.OnDblClick := @Self.RunVersion;
    fmEmutecaSoftList.SoftList := Emuteca.SoftManager.EnabledList;

    // Creating and Setting Tags
    aTabSheet := PageControl1.AddTabSheet;
    fmCHXTagTree := TfmTagTree.Create(aTabSheet);
    aTabSheet.Caption := fmCHXTagTree.Caption;  {TODO: Add Caption}
    fmCHXTagTree.Parent := aTabSheet;
    fmCHXTagTree.Folder := Emuteca.Config.TagSubFolder;
    fmCHXTagTree.OnCheckChange := @self.CheckTags;

  end;

begin
  Application.Title := Format(rsFmtApplicationTitle,
    [krsEmuteca, GetFileVersion]); // Usually is deleted in .lpr file...

  Randomize; // Who knows if it will be used...

  // Always work from program folder :P
  // TODO 3: Change for Linux... ¬_¬U
  ChDir(ProgramDirectory);
  // Used to store translations...
  if not DirectoryExistsUTF8('locale') then
    mkdir('locale');

  // Standard format setting (for .ini and other conversions)
  // This overrides user local settings that can cause errors
  StandardFormatSettings;

  // Windows Caption
  Self.Caption := Format(rsFmtWindowCaption,
    [Application.Title, Self.Caption]);

  FGUIConfig := cGUIConfig.Create(self);
  GUIConfig.LoadConfig('GUI.ini');
  IniPropStorage1.IniFileName := GUIConfig.ConfigFile;
  IniPropStorage1.Restore;

  // Creating Emuteca Core :-D
  FEmuteca := cEmuteca.Create(self);
  Emuteca.ProgressCallBack := @self.OnProgressBar;
  Emuteca.LoadConfig(GUIConfig.EmutecaIni);

  CreateFrames;

end;

procedure TfrmEmutecaMain.actEmulatorManagerExecute(Sender: TObject);
var
  aForm: TForm;
  aFrame: TfmEmutecaEmulatorManager;
begin
  Application.CreateForm(TForm, aForm);

  aform.Width := 800;
  aForm.Height := 600;
  aForm.Position := poMainFormCenter;

  aFrame := TfmEmutecaEmulatorManager.Create(aForm);
  aForm.Caption := Format(rsFmtWindowCaption,
    [Application.Title, aFrame.Caption]);
  aFrame.Parent := aForm;
  aFrame.Align := alClient;


  aFrame.IconsIni := GUIConfig.ImagesFolder + GUIConfig.IconsSubfolder +
    GUIConfig.IconsIniFile;
  aFrame.EmuManager := Emuteca.EmulatorManager;

  aForm.ShowModal;
  FreeAndNil(aForm);
end;

procedure TfrmEmutecaMain.actSaveListsExecute(Sender: TObject);
begin
  SaveEmuteca;
end;

procedure TfrmEmutecaMain.actScriptManagerExecute(Sender: TObject);
begin
  Application.CreateForm(TfrmEmutecaScriptManager, frmEmutecaScriptManager);

  frmEmutecaScriptManager.IconsIni :=
    GUIConfig.ImagesFolder + GUIConfig.IconsSubfolder +
    GUIConfig.IconsIniFile;

  frmEmutecaScriptManager.SetBaseFolder(Emuteca.Config.ScriptsFolder);
  frmEmutecaScriptManager.Emuteca := Emuteca;

  { TODO : Use Observer pattern... }
  if frmEmutecaScriptManager.ShowModal = mrOk then
  begin
    fmEmutecaSoftList.UpdateList;
    fmEmutecaParentList.UpdateList;
  end;
  FreeAndNil(frmEmutecaScriptManager);
end;

procedure TfrmEmutecaMain.actAddFolderExecute(Sender: TObject);
var
  aForm: TForm;
  aFrame: TfmEmutecaActAddFolder;
begin
  Application.CreateForm(TForm, aForm);

  aForm.Position := poMainFormCenter;

  aFrame := TfmEmutecaActAddFolder.Create(aForm);
  aForm.Caption := Format(rsFmtWindowCaption,
    [Application.Title, aFrame.Caption]);
  aFrame.Parent := aForm;
  aFrame.Align := alClient;

  aFrame.Emuteca := Emuteca;

  { TODO : Use Observer pattern... }
  if aForm.ShowModal = mrOk then
  begin
    fmEmutecaSoftList.UpdateList;
    fmEmutecaParentList.UpdateList;
  end;

  FreeAndNil(aForm);
end;

procedure TfrmEmutecaMain.actAddSoftExecute(Sender: TObject);
var
  aForm: TForm;
  aFrame: TfmActAddSoft;
begin
  Application.CreateForm(TForm, aForm);

  aForm.Position := poMainFormCenter;
  aForm.Caption := Format(rsFmtWindowCaption,
    [Application.Title, actAddSoft.Caption]);

  aFrame := TfmActAddSoft.Create(aForm);
  aFrame.Parent := aForm;
  aFrame.Align := alClient;
  aFrame.Emuteca := Emuteca;
  {
  aFrame.IconsIni := Emuteca.Config.ImagesFolder +
    Emuteca.Config.IconsSubfolder + Emuteca.Config.IconsIniFile;
  }
  aForm.AutoSize := True;

  { TODO : Use Observer pattern... }
  if aForm.ShowModal = mrOk then
  begin
    fmEmutecaSoftList.UpdateList;
    fmEmutecaParentList.UpdateList;
  end;
  FreeAndNil(aForm);
end;

procedure TfrmEmutecaMain.actSystemManagerExecute(Sender: TObject);
var
  aForm: TForm;
  aFrame: TfmEmutecaSystemManager;
begin
  Application.CreateForm(TForm, aForm);

  aform.Width := 800;
  aForm.Height := 600;
  aForm.Position := poMainFormCenter;

  aFrame := TfmEmutecaSystemManager.Create(aForm);
  aForm.Caption := Format(rsFmtWindowCaption,
    [Application.Title, aFrame.Caption]);
  aFrame.Parent := aForm;
  aFrame.Align := alClient;

  aFrame.IconsIni := GUIConfig.ImagesFolder + GUIConfig.IconsSubfolder +
    GUIConfig.IconsIniFile;
  aFrame.Emuteca := Emuteca;


  aForm.ShowModal;
  FreeAndNil(aForm);
end;

procedure TfrmEmutecaMain.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if not GUIConfig.SaveOnExit then
    Exit;

  SaveEmuteca;
  GUIConfig.SaveConfig('');
end;

procedure TfrmEmutecaMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGUIConfig);
  FreeAndNil(FEmuteca);
end;

end.
