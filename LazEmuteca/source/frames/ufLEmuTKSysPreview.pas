unit ufLEmuTKSysPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList, LCLIntf, LazFileUtils,
  ufCHXFrame, uCHXImageUtils,
  ucEmutecaSystem;

type

  { TfmLEmuTKSysPreview }

  TfmLEmuTKSysPreview = class(TfmCHXFrame)
    actOpenSystemFolder: TAction;
    ActionList: TActionList;
    ilActions: TImageList;
    bOpenSystemFolder: TSpeedButton;
    eLastTime: TEdit;
    eNTimes: TEdit;
    ePlayedTime: TEdit;
    gbxStats: TGroupBox;
    Splitter1: TSplitter;
    SysImage: TImage;
    procedure actOpenSystemFolderExecute(Sender: TObject);
  private
    FSystem: cEmutecaSystem;
    procedure SetSystem(AValue: cEmutecaSystem);

  protected
    procedure SetGUIIconsIni(AValue: string); override;
    procedure SetGUIConfigIni(AValue: string); override;
    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

  public
    property System: cEmutecaSystem read FSystem write SetSystem;
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

procedure TfmLEmuTKSysPreview.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  LoadFrameData;
end;

procedure TfmLEmuTKSysPreview.SetGUIIconsIni(AValue: string);
begin
  inherited SetGUIIconsIni(AValue);

   ReadActionsIcons(GUIIconsIni, Self.Name, ilActions, ActionList);
   FixComponentImagesFromActions(Self);
end;

procedure TfmLEmuTKSysPreview.SetGUIConfigIni(AValue: string);
begin
  inherited SetGUIConfigIni(AValue);
end;

procedure TfmLEmuTKSysPreview.ClearFrameData;
begin
  SysImage.Picture.Clear;
  ePlayedTime.Clear;
  eNTimes.Clear;
  eLastTime.Clear;
end;

procedure TfmLEmuTKSysPreview.LoadFrameData;
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

  ePlayedTime.Text := System.Stats.PlayingTimeStr;
  eNTimes.Text := System.Stats.TimesPlayedStr;
  eLastTime.Text := System.Stats.LastTimeStr;
end;

end.
