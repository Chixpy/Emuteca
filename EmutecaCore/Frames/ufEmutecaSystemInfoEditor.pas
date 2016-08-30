unit ufEmutecaSystemInfoEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, StdCtrls, EditBtn, ExtCtrls,
  uCHXStrUtils,
  ucEmutecaConfig, ucEmutecaSystem;

type

  { TfmSystemInfoEditor }

  TfmSystemInfoEditor = class(TFrame)
    eSystemIcon: TFileNameEdit;
    eSystemImage: TFileNameEdit;
    gbxImages: TGroupBox;
    iSystemIcon: TImage;
    iSystemImage: TImage;
    lSystemIcon: TLabel;
    lSystemImage: TLabel;
    procedure eSystemIconAcceptFileName(Sender: TObject; var Value: string);
    procedure eSystemImageAcceptFileName(Sender: TObject; var Value: string);
    procedure eFileButtonClick(Sender: TObject);
  private
    FConfig: cEmutecaConfig;
    FSystem: cEmutecaSystem;
    procedure SetConfig(AValue: cEmutecaConfig);
    procedure SetSystem(AValue: cEmutecaSystem);

  protected
    procedure SaveData;
    procedure UpdateData;
    procedure ClearData;
  public
    { public declarations }
    property System: cEmutecaSystem read FSystem write SetSystem;

    property Config: cEmutecaConfig read FConfig write SetConfig;
  end;

implementation

{$R *.lfm}

{ TfmSystemInfoEditor }

procedure TfmSystemInfoEditor.eSystemImageAcceptFileName(Sender: TObject;
  var Value: string);
begin
  if FileExistsUTF8(Value) then
    iSystemImage.Picture.LoadFromFile(Value);
end;

procedure TfmSystemInfoEditor.eFileButtonClick(Sender: TObject);
var
  aEFN: TFileNameEdit;
begin
  aEFN := TFileNameEdit(Sender);
  if FilenameIsAbsolute(aEFN.FileName) then
  begin
    aEFN.InitialDir := ExtractFileDir(SysPath(aEFN.FileName));
  end
  else
  begin
    if Assigned(System) then
      aEFN.InitialDir := ExtractFileDir(TrimFilename(System.BaseFolder +
        SysPath(aEFN.FileName)));
  end;
end;

procedure TfmSystemInfoEditor.eSystemIconAcceptFileName(Sender: TObject;
  var Value: string);
begin
  if FileExistsUTF8(Value) then
    iSystemIcon.Picture.LoadFromFile(Value);
end;

procedure TfmSystemInfoEditor.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;
  UpdateData;
end;

procedure TfmSystemInfoEditor.SetConfig(AValue: cEmutecaConfig);
begin
  if FConfig = AValue then
    Exit;
  FConfig := AValue;
  UpdateData;
end;

procedure TfmSystemInfoEditor.SaveData;
begin

end;

procedure TfmSystemInfoEditor.UpdateData;
begin
  ClearData;

  if not assigned(System) then
    Exit;
end;

procedure TfmSystemInfoEditor.ClearData;
begin
  eSystemImage.Clear;
  iSystemImage.Picture.Clear;
  eSystemIcon.Clear;
  iSystemIcon.Picture.Clear;
end;

end.
