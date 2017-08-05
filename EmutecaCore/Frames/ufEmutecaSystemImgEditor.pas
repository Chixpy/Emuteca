unit ufEmutecaSystemImgEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, StdCtrls,
  EditBtn, ExtCtrls, Buttons, ActnList, ComCtrls,
  uCHXStrUtils, uCHXDlgUtils,
  ufCHXPropEditor, ufCHXImgViewer,
  ucEmutecaSystem;

type

  { TfmEmuTKSystemImgEditor }

  TfmEmuTKSystemImgEditor = class(TfmCHXPropEditor)
    eSystemIcon: TFileNameEdit;
    eSystemBG: TFileNameEdit;
    eSystemImage: TFileNameEdit;
    gbxImages: TGroupBox;
    gbxSystemIcon: TGroupBox;
    gbxSystemBG: TGroupBox;
    gbxSystemImage: TGroupBox;
    iSystemIcon: TImage;
    iSystemBG: TImage;
    iSystemImage: TImage;
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
    FSHA1Folder: string;
    FSystem: cEmutecaSystem;

    procedure SetSHA1Folder(AValue: string);
    procedure SetSystem(AValue: cEmutecaSystem);

  protected
    procedure SetGUIIconsIni(AValue: string); override;
    procedure SetGUIConfigIni(AValue: string); override;

    procedure UpdateImage(aTImage: TImage; aFile: string);

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

  public
    { public declarations }
    property System: cEmutecaSystem read FSystem write SetSystem;
    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    procedure SaveFrameData; override;

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
  TfmCHXImgViewer.SimpleFormI(eSystemImage.Text, SHA1Folder,GUIIconsIni, GUIConfigIni);
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

procedure TfmEmuTKSystemImgEditor.SetGUIIconsIni(AValue: string);
begin
  inherited SetGUIIconsIni(AValue);
end;

procedure TfmEmuTKSystemImgEditor.SetGUIConfigIni(AValue: string);
begin
  inherited SetGUIConfigIni(AValue);
end;

procedure TfmEmuTKSystemImgEditor.SaveFrameData;
begin
  if not Enabled then
    Exit;

  System.Icon := eSystemIcon.Text;
  System.Image := eSystemImage.Text;
  System.BackImage := eSystemBG.Text;
end;

procedure TfmEmuTKSystemImgEditor.LoadFrameData;
begin
  Enabled := Assigned(System);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  eSystemImage.Text := System.Image;
  UpdateImage(iSystemImage, eSystemImage.Text);
  eSystemIcon.Text := System.Icon;
  UpdateImage(iSystemIcon, eSystemIcon.Text);
  eSystemBG.Text := System.BackImage;
  UpdateImage(iSystemBG, eSystemBG.Text);
end;

constructor TfmEmuTKSystemImgEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmEmuTKSystemImgEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TfmEmuTKSystemImgEditor.ClearFrameData;
begin
  eSystemImage.Clear;
  iSystemImage.Picture.Clear;
  eSystemIcon.Clear;
  iSystemIcon.Picture.Clear;
  eSystemBG.Clear;
  iSystemBG.Picture.Clear;
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
