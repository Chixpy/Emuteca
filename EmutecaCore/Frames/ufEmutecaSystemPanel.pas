unit ufEmutecaSystemPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LazFileUtils,
  ufCHXFrame,
  ucEmutecaSystem;

type

  { TfmEmutecaSystemPanel }

  TfmEmutecaSystemPanel = class(TfmCHXFrame)
    Splitter1: TSplitter;
    SysImage: TImage;
  private
    FSystem: cEmutecaSystem;
    procedure SetSystem(AValue: cEmutecaSystem);

  public
    property System: cEmutecaSystem read FSystem write SetSystem;

    procedure ClearData; override;
    procedure LoadData; override;
    procedure SaveData; override;

  end;

implementation

{$R *.lfm}

{ TfmEmutecaSystemPanel }

procedure TfmEmutecaSystemPanel.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then Exit;
  FSystem := AValue;

  LoadData;
end;

procedure TfmEmutecaSystemPanel.ClearData;
begin
  SysImage.Picture.Clear;
end;

procedure TfmEmutecaSystemPanel.LoadData;
begin
  Enabled := Assigned(System);

  if not Enabled then
  begin
    ClearData;
    Exit;
  end;

  if FileExistsUTF8(System.Image) then
    SysImage.Picture.LoadFromFile(System.Image)
  else
   SysImage.Picture.Clear;
end;

procedure TfmEmutecaSystemPanel.SaveData;
begin

end;

end.

