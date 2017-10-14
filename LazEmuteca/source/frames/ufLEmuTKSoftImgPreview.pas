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

  protected
    procedure CreateListView; override;
    function GetCaptionList: TStrings; override;
    function GetFolder: string; override;
  public
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKSoftImgPreview }

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

