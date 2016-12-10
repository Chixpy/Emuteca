unit ufEmutecaSystemImgEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, StdCtrls, EditBtn, ExtCtrls,
  uCHXStrUtils, ufCHXPropEditor,
  ucEmutecaConfig, ucEmutecaSystem;

type

  { TfmSystemImgEditor }

  TfmSystemImgEditor = class(TfmCHXPropEditor)
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
    FSystem: cEmutecaSystem;
    procedure SetSystem(AValue: cEmutecaSystem);

  protected
    procedure ClearData; override;

  public
    { public declarations }
    property System: cEmutecaSystem read FSystem write SetSystem;

    procedure SaveData; override;
    procedure LoadData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmSystemImgEditor }

procedure TfmSystemImgEditor.eSystemImageAcceptFileName(Sender: TObject;
  var Value: string);
begin
  if FileExistsUTF8(Value) then
    iSystemImage.Picture.LoadFromFile(Value);
end;

procedure TfmSystemImgEditor.eFileButtonClick(Sender: TObject);
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

procedure TfmSystemImgEditor.eSystemIconAcceptFileName(Sender: TObject;
  var Value: string);
begin
  if FileExistsUTF8(Value) then
    iSystemIcon.Picture.LoadFromFile(Value);
end;

procedure TfmSystemImgEditor.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  LoadData;

  self.Enabled := Assigned(System)
end;

procedure TfmSystemImgEditor.SaveData;
begin
  if not Assigned(System) then
    Exit;

  System.Icon := eSystemIcon.Text;
  System.Image := eSystemImage.Text;
end;

procedure TfmSystemImgEditor.LoadData;
begin
  if not Assigned(System) then
  begin
    ClearData;
    Exit;
  end;

  eSystemIcon.Text := System.Icon;
  if FileExistsUTF8(System.Icon) then
    iSystemIcon.Picture.LoadFromFile(System.Icon)
  else
    iSystemIcon.Picture.Clear;
  eSystemImage.Text := System.Image;
  if FileExistsUTF8(System.Image) then
    iSystemImage.Picture.LoadFromFile(System.Image)
  else
    iSystemImage.Picture.Clear;
end;

constructor TfmSystemImgEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmSystemImgEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TfmSystemImgEditor.ClearData;
begin
  eSystemImage.Clear;
  iSystemImage.Picture.Clear;
  eSystemIcon.Clear;
  iSystemIcon.Picture.Clear;
end;

end.
