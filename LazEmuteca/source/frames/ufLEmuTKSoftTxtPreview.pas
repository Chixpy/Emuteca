unit ufLEmuTKSoftTxtPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, 
    uafLEmuTKSoftFoldersPreview, ufCHXTxtListPreview;

type

  { TfmLEmuTKSoftTxtPreview }

  TfmLEmuTKSoftTxtPreview = class(TfmLEmuTKSoftFoldersPreview)
  private

  protected
    procedure CreateListView; override;
       function GetCaptionList: TStrings; override;
    function GetFolder: string; override;
  public

  end;

implementation

{$R *.lfm}

{ TfmLEmuTKSoftTxtPreview }

procedure TfmLEmuTKSoftTxtPreview.CreateListView;
begin
    SetListPreview(TfmCHXTxtListPreview.Create(Self));
    fmListPreview.Align := alClient;
    fmListPreview.Parent := Self;
end;

function TfmLEmuTKSoftTxtPreview.GetCaptionList: TStrings;
begin
  Result := nil;

  if Assigned(System) then
   Result := System.TextCaptions;
end;

function TfmLEmuTKSoftTxtPreview.GetFolder: string;
begin
  Result := '';

  if Assigned(System) then
   Result := System.TextFolders[cbxFolderCaption.ItemIndex];
end;

end.

