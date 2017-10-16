unit ufEmutecaSystemImgEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, StdCtrls,
  EditBtn, ExtCtrls, Buttons, ActnList, ComCtrls, inifiles,
  uCHXStrUtils, uCHXDlgUtils,
  ufCHXPropEditor, ufCHXImgViewer,
  ucEmutecaSystem;

type

  { TfmEmuTKSystemImgEditor }

  TfmEmuTKSystemImgEditor = class(TfmCHXPropEditor)
    eSoftIconFolder: TDirectoryEdit;
    eSystemIcon: TFileNameEdit;
    eSystemBG: TFileNameEdit;
    eSystemImage: TFileNameEdit;
    gbxIconFolder: TGroupBox;
    gbxImages: TGroupBox;
    gbxSystemIcon: TGroupBox;
    gbxSystemBG: TGroupBox;
    gbxSystemIcons: TGroupBox;
    gbxSystemImage: TGroupBox;
    iSystemIcon: TImage;
    iSystemBG: TImage;
    iSystemImage: TImage;
    procedure eSoftIconFolderButtonClick(Sender: TObject);
    procedure eSystemBGAcceptFileName(Sender: TObject; var Value: string);
    procedure eSystemBGButtonClick(Sender: TObject);
    procedure eSystemIconAcceptFileName(Sender: TObject; var Value: string);
    procedure eSystemIconButtonClick(Sender: TObject);
    procedure eSystemImageAcceptFileName(Sender: TObject; var Value: string);
    procedure eSystemImageButtonClick(Sender: TObject);
    procedure iSystemBGDblClick(Sender: TObject);
    procedure iSystemIconDblClick(Sender: TObject);
    procedure iSystemImageDblClick(Sender: TObject);

  private
    FGUIConfigIni: string;
    FGUIIconsIni: string;
    FSHA1Folder: string;
    FSystem: cEmutecaSystem;

    procedure SetGUIConfigIni(AValue: string);
    procedure SetGUIIconsIni(AValue: string);
    procedure SetSHA1Folder(AValue: string);
    procedure SetSystem(AValue: cEmutecaSystem);

  protected
    property GUIIconsIni: string read FGUIIconsIni write SetGUIIconsIni;
    property GUIConfigIni: string read FGUIConfigIni write SetGUIConfigIni;

    procedure UpdateImage(aTImage: TImage; aFile: string);

    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

    procedure DoLoadGUIIcons(aIconsIni: TIniFile; aBaseFolder: string); override;
    procedure DoLoadGUIConfig(aIniFile: TIniFile);

  public
    { public declarations }
    property System: cEmutecaSystem read FSystem write SetSystem;
    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmuTKSystemImgEditor }

procedure TfmEmuTKSystemImgEditor.eSystemImageAcceptFileName(Sender: TObject;
  var Value: string);
begin
  UpdateImage(iSystemImage, Value);
end;

procedure TfmEmuTKSystemImgEditor.eSystemImageButtonClick(Sender: TObject);
begin
  if Assigned(System) then
    SetFileEditInitialDir(eSystemImage, System.BaseFolder)
  else
    SetFileEditInitialDir(eSystemImage, '');
end;

procedure TfmEmuTKSystemImgEditor.iSystemBGDblClick(Sender: TObject);
begin
  TfmCHXImgViewer.SimpleFormI(eSystemBG.Text, SHA1Folder,GUIIconsIni, GUIConfigIni);
end;

procedure TfmEmuTKSystemImgEditor.iSystemIconDblClick(Sender: TObject);
begin
  TfmCHXImgViewer.SimpleFormI(eSystemIcon.Text, SHA1Folder,GUIIconsIni, GUIConfigIni);
end;

procedure TfmEmuTKSystemImgEditor.iSystemImageDblClick(Sender: TObject);
begin
  TfmCHXImgViewer.SimpleFormI(eSystemImage.Text, SHA1Folder, GUIIconsIni, GUIConfigIni);
end;

procedure TfmEmuTKSystemImgEditor.eSystemIconAcceptFileName(Sender: TObject;
  var Value: string);
begin
  UpdateImage(iSystemIcon, Value);
end;

procedure TfmEmuTKSystemImgEditor.eSystemIconButtonClick(Sender: TObject);
begin
  if Assigned(System) then
    SetFileEditInitialDir(eSystemIcon, System.BaseFolder)
  else
    SetFileEditInitialDir(eSystemIcon, '');
end;

procedure TfmEmuTKSystemImgEditor.eSystemBGAcceptFileName(Sender: TObject;
  var Value: string);
begin
  UpdateImage(iSystemBG, Value);
end;

procedure TfmEmuTKSystemImgEditor.eSoftIconFolderButtonClick(Sender: TObject);
begin
   if Assigned(System) then
    SetDirEditInitialDir(eSoftIconFolder, System.BaseFolder)
  else
    SetDirEditInitialDir(eSoftIconFolder, '');
end;

procedure TfmEmuTKSystemImgEditor.eSystemBGButtonClick(Sender: TObject);
begin
  if Assigned(System) then
    SetFileEditInitialDir(eSystemBG, System.BaseFolder)
  else
    SetFileEditInitialDir(eSystemBG, '');
end;

procedure TfmEmuTKSystemImgEditor.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  LoadFrameData;
end;

procedure TfmEmuTKSystemImgEditor.SetSHA1Folder(AValue: string);
begin
  FSHA1Folder := SetAsFolder(AValue);
end;

procedure TfmEmuTKSystemImgEditor.SetGUIConfigIni(AValue: string);
begin
  FGUIConfigIni := SetAsFile(AValue);
end;

procedure TfmEmuTKSystemImgEditor.SetGUIIconsIni(AValue: string);
begin
  FGUIIconsIni := SetAsFile(AValue);
end;

procedure TfmEmuTKSystemImgEditor.DoSaveFrameData;
begin
  if not Enabled then
    Exit;

  System.Icon := eSystemIcon.FileName;
  System.Image := eSystemImage.FileName;
  System.BackImage := eSystemBG.FileName;
  System.IconFolder := eSoftIconFolder.Directory;
end;

procedure TfmEmuTKSystemImgEditor.DoLoadGUIIcons(aIconsIni: TIniFile;
  aBaseFolder: string);
begin
  inherited DoLoadGUIIcons(aIconsIni, aBaseFolder);
  GUIIconsIni := aIconsIni.FileName;
end;

procedure TfmEmuTKSystemImgEditor.DoLoadGUIConfig(aIniFile: TIniFile);
begin
  GUIConfigIni := aIniFile.FileName;
end;

procedure TfmEmuTKSystemImgEditor.DoLoadFrameData;
begin
  Enabled := Assigned(System);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  eSystemImage.FileName := System.Image;
  UpdateImage(iSystemImage, eSystemImage.FileName);
  eSystemIcon.FileName := System.Icon;
  UpdateImage(iSystemIcon, eSystemIcon.FileName);
  eSystemBG.FileName := System.BackImage;
  UpdateImage(iSystemBG, eSystemBG.FileName);
  eSoftIconFolder.Directory := System.IconFolder;
end;

constructor TfmEmuTKSystemImgEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
  OnLoadGUIConfig := @DoLoadGUIConfig;
  OnLoadGUIIcons := @DoLoadGUIIcons;
end;

destructor TfmEmuTKSystemImgEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TfmEmuTKSystemImgEditor.DoClearFrameData;
begin
  eSystemImage.Clear;
  iSystemImage.Picture.Clear;
  eSystemIcon.Clear;
  iSystemIcon.Picture.Clear;
  eSystemBG.Clear;
  iSystemBG.Picture.Clear;
  eSoftIconFolder.Clear;
end;

procedure TfmEmuTKSystemImgEditor.UpdateImage(aTImage: TImage; aFile: string);
begin
  if not Assigned(aTImage) then
    Exit;

  if FileExistsUTF8(aFile) then
    aTImage.Picture.LoadFromFile(aFile)
  else
    aTImage.Picture.Clear;
end;

end.
