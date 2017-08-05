unit ufLEmuTKSysPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LazFileUtils, ufCHXFrame, ucEmutecaSystem;

type

  { TfmEmutecaSystemPanel }

  TfmEmutecaSystemPanel = class(TfmCHXFrame)
    eLastTime: TEdit;
    eNTimes: TEdit;
    ePlayedTime: TEdit;
    gbxStats: TGroupBox;
    Splitter1: TSplitter;
    SysImage: TImage;
  private
    FSystem: cEmutecaSystem;
    procedure SetSystem(AValue: cEmutecaSystem);

  protected
        procedure ClearFrameData; override;
    procedure LoadFrameData; override;

  public
    property System: cEmutecaSystem read FSystem write SetSystem;


  end;

implementation

{$R *.lfm}

{ TfmEmutecaSystemPanel }

procedure TfmEmutecaSystemPanel.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then Exit;
  FSystem := AValue;

  LoadFrameData;
end;

procedure TfmEmutecaSystemPanel.ClearFrameData;
begin
  SysImage.Picture.Clear;
  ePlayedTime.Clear;
  eNTimes.Clear;
  eLastTime.Clear;
end;

procedure TfmEmutecaSystemPanel.LoadFrameData;
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

