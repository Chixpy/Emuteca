unit ufEmutecaMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls,
  Graphics, Dialogs, ActnList,
  Menus, StdActns, ComCtrls, ExtCtrls, DefaultTranslator, IniPropStorage,
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
  ucEmuteca, ucEmutecaParent, ucEmutecaVersion,
  // Emuteca frames
  ufEmutecaParentList, ufEmutecaVersionList, ufEmutecaEmulatorManager,
  ufEmutecaSystemManager,
  uGUIConfig;

type

  { TfrmEmutecaMain }

  TfrmEmutecaMain = class(TForm)
    actEmulatorManager: TAction;
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
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    mmHelp: TMenuItem;
    mmFile: TMenuItem;
    MenuItem2: TMenuItem;
    PageControl1: TPageControl;
    pBottom: TPanel;
    pMiddle: TPanel;
    pRight: TPanel;
    pTop: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    stbHelp: TStatusBar;
    stbInfo: TStatusBar;
    procedure actEmulatorManagerExecute(Sender: TObject);
    procedure actSystemManagerExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpOnHelp1Execute(Sender: TObject);
  private
    { private declarations }
    FEmuteca: cEmuteca;
    FGUIConfig: cGUIConfig;

    // Frames
    fmEmutecaParentList: TfmEmutecaParentList;
    fmEmutecaVersionList: TfmEmutecaVersionList;
    fmCHXTagTree: TfmTagTree;
    procedure SetGUIConfig(AValue: cGUIConfig);

  protected
    property Emuteca: cEmuteca read FEmuteca;
    property GUIConfig: cGUIConfig read FGUIConfig write SetGUIConfig;

    procedure CheckTags(aList: TStrings);
    procedure SelectParent(const aParent: cEmutecaParent);
    procedure SelectSoftware(const aSoftware: cEmutecaVersion);
    procedure RunVersion(const aSoftware: cEmutecaVersion);

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

procedure TfrmEmutecaMain.SelectSoftware(const aSoftware: cEmutecaVersion);
begin
  Emuteca.CurrentSoft := aSoftware;
end;

procedure TfrmEmutecaMain.RunVersion(const aSoftware: cEmutecaVersion);
begin
  Emuteca.RunSoftware(aSoftware);
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

    // Creating and setting the parent list frame
    fmEmutecaParentList := TfmEmutecaParentList.Create(pTop);
    fmEmutecaParentList.Parent := pTop;
    fmEmutecaParentList.ParentList := Emuteca.ParentManager.CurrentList;
    fmEmutecaParentList.OnItemSelect := @Self.SelectParent;

    // Creating and Setting the software list frame
    fmEmutecaVersionList := TfmEmutecaVersionList.Create(pBottom);
    fmEmutecaVersionList.Parent := pBottom;
    fmEmutecaVersionList.Emuteca := Emuteca;
    fmEmutecaVersionList.OnItemSelect := @Self.SelectSoftware;
    fmEmutecaVersionList.OnDblClick := @Self.RunVersion;

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

  // Creating Emuteca Core :-D
  FEmuteca := cEmuteca.Create(self);
  Emuteca.ProgressCallBack := @self.OnProgressBar;
  Emuteca.LoadConfig(krsEmuteca + '.ini');

  FGUIConfig := cGUIConfig.Create(self);
  GUIConfig.LoadConfig('GUI.ini');

  IniPropStorage1.IniFileName := GUIConfig.ConfigFile;
  IniPropStorage1.Restore;

  CreateFrames;

end;

procedure TfrmEmutecaMain.actEmulatorManagerExecute(Sender: TObject);
var
  aForm: TForm;
  aFrame: TfmEmutecaEmulatorManager;
begin
  Application.CreateForm(TForm, aForm);

  aForm.Position := poMainFormCenter;
  aForm.AutoSize := True;

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

procedure TfrmEmutecaMain.actSystemManagerExecute(Sender: TObject);
var
  aForm: TForm;
  aFrame: TfmEmutecaSystemManager;
begin
  Application.CreateForm(TForm, aForm);

  aForm.Position := poMainFormCenter;

  aFrame := TfmEmutecaSystemManager.Create(aForm);
  aForm.Caption := Format(rsFmtWindowCaption,
    [Application.Title, aFrame.Caption]);
  aFrame.Parent := aForm;
  aFrame.Align := alClient;

  aFrame.IconsIni := GUIConfig.ImagesFolder + GUIConfig.IconsSubfolder +
    GUIConfig.IconsIniFile;
  { TODO : Assign whole Emuteca? }
  aFrame.EmuManager := Emuteca.EmulatorManager;
  aFrame.SysManager := Emuteca.SystemManager;

  aForm.ShowModal;
  FreeAndNil(aForm);
end;

procedure TfrmEmutecaMain.FormDestroy(Sender: TObject);
begin
  GUIConfig.SaveConfig('');
  FreeAndNil(FGUIConfig);
  FreeAndNil(FEmuteca);
end;

end.
