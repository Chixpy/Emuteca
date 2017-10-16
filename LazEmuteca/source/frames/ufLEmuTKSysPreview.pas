unit ufLEmuTKSysPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList, LCLIntf, ComCtrls, LazFileUtils, IniFiles,
  uCHXImageUtils, uCHXStrUtils,
  ufCHXFrame, ufCHXImgViewer,
  ucEmutecaSystem,
  uLEmuTKCommon;

type

  { TfmLEmuTKSysPreview }

  TfmLEmuTKSysPreview = class(TfmCHXFrame)
    actOpenSystemFolder: TAction;
    ActionList: TActionList;
    eNSoft: TEdit;
    eNGroups: TEdit;
    ilActions: TImageList;
    eLastTime: TEdit;
    eNTimes: TEdit;
    ePlayedTime: TEdit;
    gbxStats: TGroupBox;
    Splitter1: TSplitter;
    SysImage: TImage;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure actOpenSystemFolderExecute(Sender: TObject);
    procedure SysImageDblClick(Sender: TObject);
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

    procedure DoClearFrameData;
    procedure DoLoadFrameData;

    procedure DoLoadGUIConfig(aIniFile: TIniFile);
    procedure DoLoadGUIIcons(aIconsIni: TIniFile; aBaseFolder: string);

  public
    property System: cEmutecaSystem read FSystem write SetSystem;

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

        constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKSysPreview }

procedure TfmLEmuTKSysPreview.actOpenSystemFolderExecute(Sender: TObject);
begin
  if not Assigned(System) then
    Exit;

  OpenDocument(System.BaseFolder);
end;

procedure TfmLEmuTKSysPreview.SysImageDblClick(Sender: TObject);
begin
  if FileExistsUTF8(System.Image) then
    TfmCHXImgViewer.SimpleFormI(System.Image, SHA1Folder, GUIIconsIni, GUIConfigIni);
end;

procedure TfmLEmuTKSysPreview.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  LoadFrameData;
end;

procedure TfmLEmuTKSysPreview.DoLoadGUIIcons(aIconsIni: TIniFile;
  aBaseFolder: string);
begin
  GUIIconsIni := aIconsIni.FileName;
  ReadActionsIconsIni(aIconsIni, aBaseFolder, Self.Name, ilActions, ActionList);
  FixComponentImagesFromActions(Self);
end;

constructor TfmLEmuTKSysPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;

  OnLoadGUIIcons := @DoLoadGUIIcons;
  OnLoadGUIConfig := @DoLoadGUIConfig;
end;

destructor TfmLEmuTKSysPreview.Destroy;
begin
  inherited Destroy;
end;

procedure TfmLEmuTKSysPreview.SetSHA1Folder(AValue: string);
begin
  if FSHA1Folder = AValue then Exit;
  FSHA1Folder := AValue;
end;

procedure TfmLEmuTKSysPreview.SetGUIConfigIni(AValue: string);
begin
  FGUIConfigIni := SetAsFile(AValue);
end;

procedure TfmLEmuTKSysPreview.SetGUIIconsIni(AValue: string);
begin
  FGUIIconsIni := SetAsFile(AValue);
end;

procedure TfmLEmuTKSysPreview.DoClearFrameData;
begin
  SysImage.Picture.Clear;
  eNSoft.Clear;
  eNGroups.Clear;
  ePlayedTime.Clear;
  eNTimes.Clear;
  eLastTime.Clear;
end;

procedure TfmLEmuTKSysPreview.DoLoadFrameData;
begin
  Enabled := Assigned(System);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  if FileExistsUTF8(System.Image) then
    SysImage.Picture.LoadFromFile(System.Image)
  else
    SysImage.Picture.Clear;

  eNSoft.Text := Format(rsFmtNVersions,[System.SoftManager.FullList.Count]);
  eNGroups.Text := Format(rsFmtNGroups,[System.GroupManager.VisibleList.Count]);
  ePlayedTime.Text := System.Stats.PlayingTimeStr;
  eNTimes.Text := System.Stats.TimesPlayedStr;
  eLastTime.Text := System.Stats.LastTimeStr;

end;

procedure TfmLEmuTKSysPreview.DoLoadGUIConfig(aIniFile: TIniFile);
begin
  GUIConfigIni := aIniFile.FileName;
end;

end.
