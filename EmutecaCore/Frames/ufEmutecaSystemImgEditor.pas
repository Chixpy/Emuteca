unit ufEmutecaSystemImgEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, StdCtrls,
  EditBtn, ExtCtrls, Buttons, ActnList, ComCtrls,
  uCHXStrUtils, uCHXDlgUtils,
  ufCHXPropEditor, ufCHXForm, ufCHXImgViewer,
  ucEmutecaConfig, ucEmutecaSystem;

type

  { TfmSystemImgEditor }

  TfmSystemImgEditor = class(TfmCHXPropEditor)
    actAddImgFolder: TAction;
    actDeleteImageFolder: TAction;
    actUpdateImageFolder: TAction;
    eImageCaption: TEdit;
    eImageFolder: TDirectoryEdit;
    eSystemIcon: TFileNameEdit;
    eSystemImage: TFileNameEdit;
    gbxImageFolders: TGroupBox;
    gbxImages: TGroupBox;
    iSystemIcon: TImage;
    iSystemImage: TImage;
    lbxImageCaptions: TListBox;
    lbxImageFolders: TListBox;
    lSystemIcon: TLabel;
    lSystemImage: TLabel;
    pEditFolder: TPanel;
    pImageFolderLists: TPanel;
    Splitter1: TSplitter;
    tbImageFolderButtons: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure actAddImgFolderExecute(Sender: TObject);
    procedure actDeleteImageFolderExecute(Sender: TObject);
    procedure actUpdateImageFolderExecute(Sender: TObject);
    procedure eImageFolderAcceptDirectory(Sender: TObject; var Value: string);
    procedure eImageFolderButtonClick(Sender: TObject);
    procedure eSystemIconAcceptFileName(Sender: TObject; var Value: string);
    procedure eSystemIconButtonClick(Sender: TObject);
    procedure eSystemImageAcceptFileName(Sender: TObject; var Value: string);
    procedure eSystemImageButtonClick(Sender: TObject);
    procedure iSystemImageDblClick(Sender: TObject);
    procedure lbxImageCaptionsSelectionChange(Sender: TObject; User: boolean);
    procedure lbxImageFoldersSelectionChange(Sender: TObject; User: boolean);
  private
    FSystem: cEmutecaSystem;
    procedure SetSystem(AValue: cEmutecaSystem);

  protected
    procedure ClearData; override;

    procedure UpdateFolderData;

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
    iSystemImage.Picture.LoadFromFile(Value)
  else
    iSystemImage.Picture.Clear;
end;

procedure TfmSystemImgEditor.eSystemImageButtonClick(Sender: TObject);
begin
  if Assigned(System) then
    SetFileEditInitialDir(eSystemImage, System.BaseFolder)
  else
    SetFileEditInitialDir(eSystemImage, '');
end;

procedure TfmSystemImgEditor.iSystemImageDblClick(Sender: TObject);
var
  aForm: TfrmCHXForm;
  fmCHXImageViewer: TfmCHXImgViewer;
begin
  if not FileExistsUTF8(System.Image) then
    Exit;

  Application.CreateForm(TfrmCHXForm, aForm);
  { TODO : Uncomment this }
  // aForm.GUIConfigIni := ;
  aForm.GUIIconsIni := Self.GUIIconsIni;

  fmCHXImageViewer := TfmCHXImgViewer.Create(aForm);
  fmCHXImageViewer.IconsIniFile := Self.GUIIconsIni;
  fmCHXImageViewer.AddImage(System.Image);
  fmCHXImageViewer.Parent := aForm;

  aForm.ShowModal;

  FreeAndNil(aForm);
end;

procedure TfmSystemImgEditor.lbxImageCaptionsSelectionChange(Sender: TObject;
  User: boolean);
begin
  if not user then
    exit;
  lbxImageFolders.ItemIndex := lbxImageCaptions.ItemIndex;
  UpdateFolderData;

end;

procedure TfmSystemImgEditor.lbxImageFoldersSelectionChange(Sender: TObject;
  User: boolean);
begin
  if not user then
    exit;
  lbxImageCaptions.ItemIndex := lbxImageFolders.ItemIndex;
  UpdateFolderData;
end;

procedure TfmSystemImgEditor.eSystemIconAcceptFileName(Sender: TObject;
  var Value: string);
begin
  if FileExistsUTF8(Value) then
    iSystemIcon.Picture.LoadFromFile(Value)
  else
    iSystemIcon.Picture.Clear;
end;

procedure TfmSystemImgEditor.eSystemIconButtonClick(Sender: TObject);
begin
  if Assigned(System) then
    SetFileEditInitialDir(eSystemIcon, System.BaseFolder)
  else
    SetFileEditInitialDir(eSystemIcon, '');
end;

procedure TfmSystemImgEditor.eImageFolderAcceptDirectory(Sender: TObject;
  var Value: string);
begin
  if DirectoryExistsUTF8(Value) then
    eImageCaption.Text := ExtractFileName(ExcludeTrailingPathDelimiter(Value))
  else
    eImageCaption.Clear;
end;

procedure TfmSystemImgEditor.actAddImgFolderExecute(Sender: TObject);
var
  aStr: string;
begin
  if not Assigned(System) then
    Exit;

  if eImageFolder.Directory = '' then
    Exit;

  lbxImageFolders.Items.Add(SetAsFolder(eImageFolder.Directory));


  if eImageCaption.Text = '' then
  begin
    lbxImageCaptions.Items.Add(
      ExtractFileName(ExcludeTrailingPathDelimiter(eImageFolder.Directory)));
  end
  else
  begin
    lbxImageCaptions.Items.Add(eImageCaption.Text);
  end;
end;

procedure TfmSystemImgEditor.actDeleteImageFolderExecute(Sender: TObject);
begin
  if not Assigned(System) then
    Exit;
  if lbxImageFolders.ItemIndex = -1 then
    exit;

  lbxImageCaptions.Items.Delete(lbxImageFolders.ItemIndex);
  lbxImageFolders.Items.Delete(lbxImageFolders.ItemIndex);
end;

procedure TfmSystemImgEditor.actUpdateImageFolderExecute(Sender: TObject);
var
  aPos: integer;
begin
  if not Assigned(System) then
    Exit;

  if eImageFolder.Directory = '' then
    Exit;

  aPos := lbxImageFolders.ItemIndex;
  if aPos = -1 then
    exit;

  lbxImageFolders.Items.BeginUpdate;
  lbxImageFolders.Items.Insert(aPos, eImageFolder.Directory);
  lbxImageFolders.Items.Delete(aPos + 1);
  lbxImageFolders.Items.EndUpdate;

  lbxImageCaptions.Items.BeginUpdate;
  if eImageCaption.Text = '' then
  begin
    lbxImageCaptions.Items.Insert(aPos,
      ExtractFileName(ExcludeTrailingPathDelimiter(eImageFolder.Directory)));
  end
  else
  begin
    lbxImageCaptions.Items.Insert(aPos, eImageCaption.Text);
  end;
  lbxImageCaptions.Items.Delete(aPos + 1);
  lbxImageCaptions.Items.EndUpdate;
end;

procedure TfmSystemImgEditor.eImageFolderButtonClick(Sender: TObject);
begin
  if Assigned(System) then
    SetDirEditInitialDir(eImageFolder, System.BaseFolder)
  else
    SetDirEditInitialDir(eImageFolder, '');
end;

procedure TfmSystemImgEditor.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  LoadData;

  self.Enabled := Assigned(System);
end;

procedure TfmSystemImgEditor.SaveData;
begin
  if not Assigned(System) then
    Exit;

  System.Icon := eSystemIcon.Text;
  System.Image := eSystemImage.Text;
  System.ImageFolders.AddStrings(lbxImageFolders.Items, True);
  System.ImageCaptions.AddStrings(lbxImageCaptions.Items, True);
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

  lbxImageFolders.Items.AddStrings(System.ImageFolders, True);
  lbxImageCaptions.Items.AddStrings(System.ImageCaptions, True);
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
  lbxImageFolders.Clear;
  lbxImageCaptions.Clear;
  eImageFolder.Clear;
  eImageCaption.Clear;
end;

procedure TfmSystemImgEditor.UpdateFolderData;
begin
  if lbxImageFolders.ItemIndex <> -1 then
    eImageFolder.Directory := lbxImageFolders.Items[lbxImageFolders.ItemIndex]
  else
    eImageFolder.Clear;

  if lbxImageCaptions.ItemIndex <> -1 then
    eImageCaption.Text := lbxImageCaptions.Items[lbxImageCaptions.ItemIndex]
  else
    eImageCaption.Clear;
end;

end.
