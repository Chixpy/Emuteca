unit ufLEmuTKSoftImgPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  uafLEmuTKSoftFoldersPreview, ufCHXImgListPreview;

type

  { TfmLEmuTKSoftImgPreview }

  TfmLEmuTKSoftImgPreview = class(TfmLEmuTKSoftFoldersPreview)
  private
    FSHA1Folder: string;
    procedure SetSHA1Folder(AValue: string);

  protected
    procedure CreateListView; override;
    function GetCaptionList: TStrings; override;
    function GetFolder: string; override;

  public
    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKSoftImgPreview }

procedure TfmLEmuTKSoftImgPreview.SetSHA1Folder(AValue: string);
begin
  if FSHA1Folder = AValue then Exit;
  FSHA1Folder := AValue;

  TfmCHXImgListPreview(fmListPreview).SHA1Folder := SHA1Folder;
end;

procedure TfmLEmuTKSoftImgPreview.CreateListView;
begin
  SetListPreview(TfmCHXImgListPreview.Create(Self));
  fmListPreview.Align := alClient;
  fmListPreview.Parent := Self;
end;

function TfmLEmuTKSoftImgPreview.GetCaptionList: TStrings;
begin
  Result := nil;

  if Assigned(System) then
   Result := System.ImageCaptions;
end;

function TfmLEmuTKSoftImgPreview.GetFolder: string;
begin
  Result := '';

  if Assigned(System) then
   Result := System.ImageFolders[cbxFolderCaption.ItemIndex];
end;


end.

