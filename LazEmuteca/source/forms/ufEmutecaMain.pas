unit ufEmutecaMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls,
  Graphics, Dialogs, LazUTF8,
  ActnList, Menus, StdActns, ComCtrls, ExtCtrls, DefaultTranslator,
  IniPropStorage, StdCtrls,
  // Misc
  uVersionSupport,
  // CHX units
  uCHXStrUtils, uCHXFileUtils, ucCHXImageList,
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
  ufEmutecaParentList, ufEmutecaIcnSoftList,
  ufEmutecaSystemCBX, ufEmutecaSoftEditor,
  // Emuteca windows
  ufEmutecaActAddSoft, ufEmutecaActAddFolder,
  // LazEmuteca frames
  ufLEmuTKSysManager, ufLEmuTKEmuManager,
  uGUIConfig;

type

  { TfrmEmutecaMain }

  TfrmEmutecaMain = class(TForm)
    actEmulatorManager: TAction;
    actAddFolder: TAction;
    actAddSoft: TAction;
    actAutoSave: TAction;
    actSaveLists: TAction;
    actMediaManager: TAction;
    actScriptManager: TAction;
    actSystemManager: TAction;
    ActionList1: TActionList;
    eSearch: TEdit;
    FileExit1: TFileExit;
    HelpOnHelp1: THelpOnHelp;
    ImageList1: TImageList;
    IniPropStorage1: TIniPropStorage;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mmHelp: TMenuItem;
    mmFile: TMenuItem;
    pcLeft: TPageControl;
    pBottom: TPanel;
    pcSoftware: TPageControl;
    pMiddle: TPanel;
    pSystems: TPanel;
    pTop: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    stbHelp: TStatusBar;
    stbInfo: TStatusBar;
    procedure actAddSoftExecute(Sender: TObject);
    procedure actAddFolderExecute(Sender: TObject);
    procedure actAutoSaveExecute(Sender: TObject);
    procedure actEmulatorManagerExecute(Sender: TObject);
    procedure actSaveListsExecute(Sender: TObject);
    procedure actScriptManagerExecute(Sender: TObject);
    procedure actSystemManagerExecute(Sender: TObject);
    procedure eSearchEditingDone(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpOnHelp1Execute(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);

  private
    FVerIcons: cCHXImageList;
    { private declarations }
    FEmuteca: cEmuteca;
    FGUIConfig: cGUIConfig;
    FIconList: cCHXImageList;

    // Frames
    fmEmutecaSystemCBX: TfmEmutecaSystemCBX;
    fmEmutecaParentList: TfmEmutecaParentList;
    fmEmutecaSoftList: TfmEmutecaIcnSoftList;

    fmCHXTagTree: TfmTagTree;

    fmEmutecaSoftEditor: TfmEmutecaSoftEditor;
    FZoneIcons: cCHXImageMap;

  protected
    property Emuteca: cEmuteca read FEmuteca;
    property GUIConfig: cGUIConfig read FGUIConfig;

    procedure CheckTags(aList: TStrings);

    function SelectSystem(aSystem: cEmutecaSystem): boolean;
    function SelectParent(aParent: cEmutecaParent): boolean;
    function SelectSoftware(aSoftware: cEmutecaSoftware): boolean;
    function RunVersion(aSoftware: cEmutecaSoftware): boolean;

    procedure SaveEmuteca;

    property IconList: cCHXImageList read FIconList;
    property VerIcons: cCHXImageList read FVerIcons;
    property ZoneIcons: cCHXImageMap read FZoneIcons;

    function AddZoneIcon(aFolder: string; FileInfo: TSearchRec): boolean;
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
  i: longint;
begin
  Temp := TStringList.Create;
  temp.Capacity := 500000;

  for i := 1 to 500000 do
  begin
    str := IntToStr(i) + ',';
    str := str + str + str + str + str + str + str + str +
      str + str + str + str + str + str + str + str + str +
      str + str + str + str + str + str + str + str;
    Temp.Add(str);
  end;

  Temp.SaveToFile('Soft.csv');
  FreeAndNil(Temp);
end;

procedure TfrmEmutecaMain.CheckTags(aList: TStrings);
begin
  { TODO : Pasar la lista para filtrar los padres y los juegos }
end;

function TfrmEmutecaMain.SelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  Result := True;

  Emuteca.CurrentSystem := aSystem;

  { TODO : Use Observer pattern... }
  fmEmutecaParentList.UpdateList;
  SelectParent(nil);
end;

function TfrmEmutecaMain.SelectParent(aParent: cEmutecaParent): boolean;
begin
  Result := True;

  Emuteca.CurrentParent := aParent;

  { TODO : Use Observer pattern... }
  fmEmutecaSoftList.UpdateList;
  SelectSoftware(nil);
end;

function TfrmEmutecaMain.SelectSoftware(aSoftware: cEmutecaSoftware): boolean;
begin
  Result := True;

  { TODO : Use Observer pattern... }
  Emuteca.CurrentSoft := aSoftware;
  fmEmutecaSoftEditor.Software := aSoftware;
end;

function TfrmEmutecaMain.RunVersion(aSoftware: cEmutecaSoftware): boolean;
begin
  Result := True;

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
  // Really, we can asume that frmCHXProgressBar is always created...;
  if not Assigned(frmCHXProgressBar) then
    Application.CreateForm(TfrmCHXProgressBar, frmCHXProgressBar);
  Result := frmCHXProgressBar.UpdTextAndBar(Title, Info1, Info2,
    Value, MaxValue);
end;

function TfrmEmutecaMain.AddZoneIcon(aFolder: string;
  FileInfo: TSearchRec): boolean;
begin
   Result := True; // Don't Stop

  // Testing extension
  if Emuteca.Config.ImageExtensions.IndexOf(UTF8LowerCase(UTF8Copy(
    ExtractFileExt(FileInfo.Name), 2, MaxInt))) = -1 then Exit;


  ZoneIcons.AddImageFile(UTF8LowerCase(ExtractFileNameOnly(FileInfo.Name)),
    aFolder + FileInfo.Name);

end;

procedure TfrmEmutecaMain.FormCreate(Sender: TObject);

  procedure LoadIcons;

    procedure AddIcon(aImageList: cCHXImageList; aIconFile: string);
    begin
      if FileExistsUTF8(aIconFile) then
        aImageList.AddImageFile(aIconFile)
      else
        aImageList.AddEmptyImage;
    end;

  var
    aFolder, aFile: string;
  begin
   {   TmpStr := Config.ImagesFolder + Config.IconsSubfolder + Config.IconsIniFile;

      // Icons for menus (without assigned TAction)
      ReadMenuIcons(TmpStr, Self.Name, '', ilActions, pmMainMenu);
      ReadMenuIcons(TmpStr, Self.Name, '', ilActions, pmSystemImage);
      ReadMenuIcons(TmpStr, Self.Name, '', ilActions, pmGameImage);
      ReadMenuIcons(TmpStr, Self.Name, '', ilActions, pmGameList);

      // Icons for TActions
      ReadActionsIcons(TmpStr, Self.Name, '', ilActions, ActionList);
      }
    // Zone icons
    aFolder := GUIConfig.ZoneIcnFolder;
    IterateFolderObj(aFolder, @AddZoneIcon, False);


   { Icons for games parents and software, first default one
      0: Default for software
    }
    aFile := GUIConfig.DefImgFolder + 'DefSoftIcon.png';
    AddIcon(IconList, aFile);

    { Icons for "flags" column, see ufEmutecaIcnSoftList.LazEmuTKIconFiles
      0: Verified.png
      1: GoodDump.png
      2: Alternate.png
      3: OverDump.png
      4: BadDump.png
      5: UnderDump.png
      6  Fixed.png
      7: Trainer.png
      8: Translation.png
      9: Pirate.png
      10: Cracked.png
      11: Modified.png
      12: Hack.png
      }
    aFolder := GUIConfig.DumpIcnFolder;
    for aFile in LazEmuTKIconFiles do
      AddIcon(FVerIcons, aFolder + aFile + '.png');
  end;

  procedure CreateFrames;
  var
    aTabSheet: TTabSheet;
  begin
    // Better create frames in code while developing...
    //   IDE has many problems updating inherited properties

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
    fmEmutecaSoftList := TfmEmutecaIcnSoftList.Create(pBottom);
    fmEmutecaSoftList.Parent := pBottom;
    fmEmutecaSoftList.SoftIconList := IconList;
    fmEmutecaSoftList.DumpIconList := VerIcons;
    fmEmutecaSoftList.ZoneIconMap := ZoneIcons;
    fmEmutecaSoftList.OnItemSelect := @Self.SelectSoftware;
    fmEmutecaSoftList.OnDblClick := @Self.RunVersion;
    fmEmutecaSoftList.SoftList := Emuteca.SoftManager.VisibleList;

    // Creating and Setting Tags frame
    aTabSheet := pcLeft.AddTabSheet;
    fmCHXTagTree := TfmTagTree.Create(aTabSheet);
    aTabSheet.Caption := fmCHXTagTree.Caption;  {TODO: Add Caption}
    fmCHXTagTree.Parent := aTabSheet;
    fmCHXTagTree.Folder := Emuteca.Config.TagSubFolder;
    fmCHXTagTree.OnCheckChange := @self.CheckTags;

    // Creating SoftEditor frame
    aTabSheet := pcSoftware.AddTabSheet;
    fmEmutecaSoftEditor := TfmEmutecaSoftEditor.Create(aTabSheet);
    aTabSheet.Caption := fmEmutecaSoftEditor.Caption;  {TODO: Add Caption}
    fmEmutecaSoftEditor.Align := alClient;
    fmEmutecaSoftEditor.Parent := aTabSheet;
    fmEmutecaSoftEditor.Emuteca := Emuteca;
    fmEmutecaSoftEditor.SaveButtons := True;
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

  // Image lists
  FIconList := cCHXImageList.Create(True);
  FVerIcons := cCHXImageList.Create(True);
  FZoneIcons := cCHXImageMap.Create(True);

  // Creating Emuteca Core :-D
  FEmuteca := cEmuteca.Create(self);
  Emuteca.ProgressCallBack := @self.OnProgressBar;
  Emuteca.LoadConfig(GUIConfig.EmutecaIni);

  LoadIcons;
  CreateFrames;


  // Misc
  { TODO : Select last system }
  actAutoSave.Checked := GUIConfig.SaveOnExit; // TODO: Use IniPropStorage1?

end;

procedure TfrmEmutecaMain.actEmulatorManagerExecute(Sender: TObject);
var
  aForm: TForm;
  aFrame: TfmLEmuTKEmuManager;
begin
  Application.CreateForm(TForm, aForm);

  // TODO: Save in GUI.ini
  aform.Width := 640;
  aForm.Height := 480;
  aForm.Position := poMainFormCenter;

  aFrame := TfmLEmuTKEmuManager.Create(aForm);
  aForm.Caption := Format(rsFmtWindowCaption,
    [Application.Title, aFrame.Caption]);
  aFrame.Parent := aForm;
  aFrame.Align := alClient;


  aFrame.IconsIni := GUIConfig.GUIIcnFile;
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

  frmEmutecaScriptManager.IconsIni := GUIConfig.GUIIcnFile;

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

procedure TfrmEmutecaMain.actAutoSaveExecute(Sender: TObject);
begin
  GUIConfig.SaveOnExit := actAutoSave.Checked;
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
  aFrame: TfmLEmuTKSysManager;
begin
  Application.CreateForm(TForm, aForm);

  // TODO: Save in GUI.ini
  aform.Width := 640;
  aForm.Height := 480;
  aForm.Position := poMainFormCenter;

  aFrame := TfmLEmuTKSysManager.Create(aForm);
  aForm.Caption := Format(rsFmtWindowCaption,
    [Application.Title, aFrame.Caption]);
  aFrame.Parent := aForm;
  aFrame.Align := alClient;

  aFrame.IconsIni := GUIConfig.GUIIcnFile;
  aFrame.Emuteca := Emuteca;

  aForm.ShowModal;
  FreeAndNil(aForm);
end;

procedure TfrmEmutecaMain.eSearchEditingDone(Sender: TObject);
begin
  //if assigned(fmEmutecaParentList) then
  // fmEmutecaParentList.FilterStr := eSearch.Text;
  if assigned(fmEmutecaSoftList) then
    fmEmutecaSoftList.FilterStr := eSearch.Text;
end;

procedure TfrmEmutecaMain.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  GUIConfig.SaveConfig('');
  if not GUIConfig.SaveOnExit then
    Exit;

  SaveEmuteca;
end;

procedure TfrmEmutecaMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FZoneIcons);
  FreeAndNil(FVerIcons);
  FreeAndNil(FIconList);
  FreeAndNil(FGUIConfig);
  FreeAndNil(FEmuteca);
end;

end.
