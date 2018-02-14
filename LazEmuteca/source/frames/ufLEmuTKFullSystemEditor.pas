unit ufLEmuTKFullSystemEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, ActnList, LazFileUtils, LCLIntf, StdCtrls,
  // CHX units
  uCHXStrUtils,
  // CHX forms
  ufrCHXForm,
  // CHX frames
  ufCHXPropEditor,
  // Emuteca common
  uEmutecaCommon,
  // Emuteca clases
  ucEmuteca, ucEmutecaSystem,
  // Emuteca frames
  ufEmutecaSystemEditor, ufEmutecaSystemImgEditor,
  ufEmutecaSystemITFEditor, ufEmutecaSystemMVFEditor,
  // LazEmuteca units
  uLEmuTKCommon;

type

  { TfmLEmuTKFullSystemEditor }

  TfmLEmuTKFullSystemEditor = class(TfmCHXPropEditor)
    actCreateFolders: TAction;
    actOpenSystemFolder: TAction;
    pcProperties: TPageControl;
    ToolBar1: TToolBar;
    bCreateFolders: TToolButton;
    bOpenSystemFolder: TToolButton;
    bSaveSystem: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure actCreateFoldersExecute(Sender: TObject);
    procedure actOpenSystemFolderExecute(Sender: TObject);

  private
    FEmuteca: cEmuteca;
    FfmSysITFEditor: TfmEmutecaSystemITFEditor;
    FfmSysEditor: TfmEmutecaSystemEditor;
    FfmSysImgEditor: TfmEmuTKSystemImgEditor;
    FfmSysMVFEditor: TfmEmutecaSystemMVFEditor;
    FSHA1Folder: string;
    FSystem: cEmutecaSystem;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetSHA1Folder(AValue: string);
    procedure SetSystem(AValue: cEmutecaSystem);

  protected
    property fmSysEditor: TfmEmutecaSystemEditor read FfmSysEditor;
    property fmSysImgEditor: TfmEmuTKSystemImgEditor read FfmSysImgEditor;
    property fmSysITFEditor: TfmEmutecaSystemITFEditor read FfmSysITFEditor;
    property fmSysMVFEditor: TfmEmutecaSystemMVFEditor read FfmSysMVFEditor;

    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DOSaveFrameData;


  public
    { public declarations }
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
    property System: cEmutecaSystem read FSystem write SetSystem;

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    class function SimpleForm(aEmuteca: cEmuteca; aSystem: cEmutecaSystem; aSHA1Folder: string;
      aGUIIconsIni: string; aGUIConfigIni: string): integer;
    //< Creates a form with System Manager.

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKFullSystemEditor }

procedure TfmLEmuTKFullSystemEditor.actCreateFoldersExecute(Sender: TObject);

  procedure CreateFolder(aSystem: cEmutecaSystem; aLine: TStringList);
  var
    aFolder: string;
    aTitle: string;
  begin
    if aLine.Count = 0 then
      Exit;

    if aLine[0] = '' then
      Exit;

    aFolder := SetAsFolder(aSystem.BaseFolder) + SetAsFolder(aLine[0]);
    ForceDirectoriesUTF8(aFolder);

    if aLine.Count = 1 then
      Exit;

    if aLine[1] = '' then
      Exit;

    if (aLine.Count = 2) or (aLine[2] = '') then
      aTitle := ExtractFileNameOnly(ExcludeTrailingPathDelimiter(aLine[0]))
    else
      aTitle := aLine[2];

    if aLine[1] = 'i' then
    begin
      aSystem.ImageFolders.Add(aFolder);
      aSystem.ImageCaptions.Add(aTitle);
    end
    else if aLine[1] = 't' then
    begin
      aSystem.TextFolders.Add(aFolder);
      aSystem.TextCaptions.Add(aTitle);
    end
    else if aLine[1] = 'm' then
    begin
      aSystem.MusicFolders.Add(aFolder);
      aSystem.MusicCaptions.Add(aTitle);
    end
    else if aLine[1] = 'v' then
    begin
      aSystem.VideoFolders.Add(aFolder);
      aSystem.VideoCaptions.Add(aTitle);
    end
    else if aLine[1] = 'c' then
    begin
      aSystem.IconFolder := aFolder;
    end;
  end;

var
  FolderList, aLine: TStringList;
  i: integer;
  TmpSys: cEmutecaSystem;
begin
  if not assigned(Emuteca) then
    Exit;
  if not Assigned(System) then
    Exit;

  // TODO: May be change the logic here:
  //  1. Test if system have changes
  //  2. Ask for save them

  // System will changes internally, so we unload it first.
  SaveFrameData; // We must save first
  TmpSys := System;
  System := nil;

  if (TmpSys.BaseFolder = '') or not DirectoryExistsUTF8(TmpSys.BaseFolder) then
  begin
    { TODO : Exception :-P }
    ShowMessageFmt(rsFmtNotFound, [GetCurrentDirUTF8, TmpSys.BaseFolder]);
    Exit;
  end;
  if not FileExistsUTF8(Emuteca.Config.AutoSysFolder) then
  begin
    { TODO : Exception :-P }
    ShowMessageFmt(rsFmtNotFound, [GetCurrentDirUTF8, Emuteca.Config.AutoSysFolder]);
    Exit;
  end;

  if MessageDlg(rsWarning, Format(rsAutoFolderWarning, [TmpSys.BaseFolder]),
    mtWarning, [mbOK, mbCancel],
    'CreateSystemFolders') = mrCancel then
    Exit;

  TmpSys.IconFolder := '';
  TmpSys.ImageFolders.Clear;
  TmpSys.ImageCaptions.Clear;
  TmpSys.TextFolders.Clear;
  TmpSys.TextCaptions.Clear;
  TmpSys.MusicFolders.Clear;
  TmpSys.MusicCaptions.Clear;
  TmpSys.VideoFolders.Clear;
  TmpSys.VideoCaptions.Clear;


  aLine := TStringList.Create;
  FolderList := TStringList.Create;
  try
    FolderList.LoadFromFile(Emuteca.Config.AutoSysFolder);
    i := 1; //Skip header
    while i < FolderList.Count do
    begin
      aLine.Clear;
      aLine.CommaText := FolderList[i];
      CreateFolder(TmpSys, aLine);

      Inc(i);
    end;

  finally
    FreeAndNil(FolderList);
    FreeAndNil(aLine);
  end;

  System := TmpSys;
end;

procedure TfmLEmuTKFullSystemEditor.actOpenSystemFolderExecute(Sender: TObject);
begin
  if (not Assigned(System)) or (not DirectoryExistsUTF8(System.BaseFolder)) then Exit;

  OpenDocument(System.BaseFolder);
end;

procedure TfmLEmuTKFullSystemEditor.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  if not Assigned(Emuteca) then
  begin
    fmSysEditor.EmuManager := nil;
  end
  else
  begin
    fmSysEditor.EmuManager := Emuteca.EmulatorManager;
  end;

  LoadFrameData;
end;

procedure TfmLEmuTKFullSystemEditor.SetSHA1Folder(AValue: string);
begin
  FSHA1Folder := SetAsFolder(AValue);
  fmSysImgEditor.SHA1Folder := SHA1Folder;
end;

procedure TfmLEmuTKFullSystemEditor.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  fmSysEditor.System := System;
  fmSysImgEditor.System := System;
  fmSysITFEditor.System := System;
  fmSysMVFEditor.System := System;

  LoadFrameData;
end;

procedure TfmLEmuTKFullSystemEditor.DoClearFrameData;
begin
end;

procedure TfmLEmuTKFullSystemEditor.DOSaveFrameData;
begin
  fmSysEditor.SaveFrameData;
  fmSysImgEditor.SaveFrameData;
  fmSysITFEditor.SaveFrameData;
  fmSysMVFEditor.SaveFrameData;
end;

class function TfmLEmuTKFullSystemEditor.SimpleForm(aEmuteca: cEmuteca;
  aSystem: cEmutecaSystem; aSHA1Folder: string; aGUIIconsIni: string;
  aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmLEmuTKFullSystemEditor;
begin
  Result := mrNone;

  if (not assigned(aEmuteca)) or (not assigned(aSystem)) then
  begin
    Result := mrAbort;
    Exit;
  end;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmLEmuTKSysEditor';
    aForm.Caption := Format(krsFmtWindowCaption,
      [Application.Title, 'System Editor']);

    aFrame := TfmLEmuTKFullSystemEditor.Create(aForm);
    aFrame.SaveButtons := True;
    aFrame.ButtonClose := True;
    aFrame.Align := alClient;

    aFrame.SHA1Folder := aSHA1Folder;
    aFrame.Emuteca := aEmuteca;
    aFrame.System := aSystem;

    aForm.LoadGUIConfig(aGUIConfigIni);
    aForm.LoadGUIIcons(aGUIIconsIni);
    aFrame.Parent := aForm;

    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;
end;

procedure TfmLEmuTKFullSystemEditor.DoLoadFrameData;
begin
  Enabled := Assigned(Emuteca) and Assigned(System);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

constructor TfmLEmuTKFullSystemEditor.Create(TheOwner: TComponent);

  procedure CreatePages;
  var
    aTabSheet: TTabSheet;
  begin
    aTabSheet := pcProperties.AddTabSheet;
    aTabSheet.Caption := 'Basic config';
    FfmSysEditor := TfmEmutecaSystemEditor.Create(aTabSheet);
    fmSysEditor.SaveButtons := False;
    fmSysEditor.ButtonClose := False;
    fmSysEditor.Align := alClient;
    fmSysEditor.Parent := aTabSheet;

    aTabSheet := pcProperties.AddTabSheet;
    aTabSheet.Caption := 'Basic images';
    FfmSysImgEditor := TfmEmuTKSystemImgEditor.Create(aTabSheet);
    fmSysImgEditor.SaveButtons := False;
    fmSysImgEditor.ButtonClose := False;
    fmSysImgEditor.Align := alClient;
    fmSysImgEditor.Parent := aTabSheet;

    aTabSheet := pcProperties.AddTabSheet;
    FfmSysITFEditor := TfmEmutecaSystemITFEditor.Create(aTabSheet);
    aTabSheet.Caption := 'Software images';
    fmSysITFEditor.SaveButtons := False;
    fmSysITFEditor.ButtonClose := False;
    fmSysITFEditor.Align := alClient;
    fmSysITFEditor.Parent := aTabSheet;

    aTabSheet := pcProperties.AddTabSheet;
    aTabSheet.Caption := 'Music & Video';
    FfmSysMVFEditor := TfmEmutecaSystemMVFEditor.Create(aTabSheet);
    fmSysMVFEditor.SaveButtons := False;
    fmSysMVFEditor.ButtonClose := False;
    fmSysMVFEditor.Align := alClient;
    fmSysMVFEditor.Parent := aTabSheet;
  end;

begin
  inherited Create(TheOwner);

  CreatePages;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmLEmuTKFullSystemEditor.Destroy;
begin
  inherited Destroy;
end;

end.
