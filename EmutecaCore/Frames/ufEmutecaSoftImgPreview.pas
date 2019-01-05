unit ufEmutecaSoftImgPreview;

{< TfmEmutecaSoftImgPreview frame unit.

  This file is part of Emuteca GUI.

  Copyright (C) 2018-2018 Chixpy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ActnList, ExtCtrls, LCLType, Clipbrd, Menus, LazFileUtils,
  // CHX units
  uCHXStrUtils,
  // CHX frames
  ufCHXImgListPreview,
  // Emuteca Core units
  uEmutecaRscStr;

type

  { TfmEmutecaSoftImgPreview }

  TfmEmutecaSoftImgPreview = class(TfmCHXImgListPreview)
    actDeleteImage: TAction;
    actAddImageFromClpBrd: TAction;
    actReplaceImageFromClpBrd: TAction;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mipmImgPasteImage: TMenuItem;
    pmImageActions: TPopupMenu;
    tbPasteImage: TToolButton;
    procedure actAddImageFromClpBrdExecute(Sender: TObject);
    procedure actDeleteImageExecute(Sender: TObject);
    procedure actReplaceImageFromClpBrdExecute(Sender: TObject);

  private
    FSaveImageFolder: string;
    procedure SetSaveImageFolder(const aSaveImageFolder: string);

  protected
    procedure DoClearFrameData; override;

    function LoadImageFromClpBrd: boolean;
    procedure SaveImageToFile(aFile: string);

  public
    property SaveImageFolder: string read FSaveImageFolder
      write SetSaveImageFolder;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

{ TfmEmutecaSoftImgPreview }

procedure TfmEmutecaSoftImgPreview.actAddImageFromClpBrdExecute(
  Sender: TObject);
var
  DlgResult: integer;
  aFileName: string;
begin
  if SaveImageFolder = '' then
    Exit;

  if not LoadImageFromClpBrd then
    Exit;

  // TODO: If a software is selected ask if it must be assigned to the parent
  //   or the software itself... But, how can we check it?

  // Unique filename
  aFileName := FormatDateTime('yyyymmddhhnnss', Now);
  // Choosing format
  DlgResult := MessageDlg(rsChooseImageFileFormat, mtConfirmation,
    [mbYes, mbNo, mbCancel], 0);
  case DlgResult of
    mrYes: aFileName := aFileName + '.png';
    mrNo: aFileName := aFileName + '.jpg';
    else
      Exit;
  end;
  aFileName := SaveImageFolder + aFileName;

  SaveImageToFile(aFileName);

  FileList.Add(aFileName);
end;

procedure TfmEmutecaSoftImgPreview.actDeleteImageExecute(Sender: TObject);
begin
  if not (ItemIndex in [0..ItemCount]) then
    Exit;

  // TODO: <game>.zip/file.png test....

  if MessageDlg(Format(rsCorfirmDeleteFile, [FileList[ItemIndex]]),
    mtConfirmation, [mbYes, mbNo], -1) = mrNo then
    Exit;

  if not DeleteFileUTF8(FileList[ItemIndex]) then
  begin
    ShowMessageFmt(rsErrorDeletingFile, [FileList[ItemIndex]]);
    Exit;
  end;

  FileList.Delete(ItemIndex);
end;

procedure TfmEmutecaSoftImgPreview.actReplaceImageFromClpBrdExecute(
  Sender: TObject);
begin
  if (ItemIndex in [0..ItemCount]) then
  begin
    // TODO: <game>.zip/file.png test....

    if not LoadImageFromClpBrd then
      Exit;

    SaveImageToFile(FileList[ItemIndex]);
  end
  else
    actAddImageFromClpBrd.Execute;
end;

procedure TfmEmutecaSoftImgPreview.SetSaveImageFolder(
  const aSaveImageFolder: string);
begin
  FSaveImageFolder := SetAsFolder(aSaveImageFolder);

  Enabled := SaveImageFolder <> '';
end;

procedure TfmEmutecaSoftImgPreview.DoClearFrameData;
begin
  inherited DoClearFrameData;

  // Enabling buttons because a image can be added.
  Enabled := SaveImageFolder <> '';
end;

function TfmEmutecaSoftImgPreview.LoadImageFromClpBrd: boolean;
var
  CF: TClipboardFormat;
begin
  Result := False;

  // Checking clipboard format
  // TODO: Make an exception.
  CF := Clipboard.FindPictureFormatID;
  if CF = 0 then // There isn't an image in clipboard
  begin
    ShowMessage('Image format not recognized.');
    Exit;
  end;

  // Loading image directly to iImage component
  // TODO: Is this needed?
  //if CF = Windows.CF_BITMAP then // Handle CF_BITMAP separately
  //  aPicture.LoadFromClipboardFormat(PredefinedClipboardFormat(
  //    pcfDelphiBitmap))
  //else
  iImage.Picture.LoadFromClipboardFormat(CF);

  Result := True;
end;

procedure TfmEmutecaSoftImgPreview.SaveImageToFile(aFile: string);
begin
  // Checking if file already exists...
  if FileExistsUTF8(aFile) then
  begin
    if MessageDlg(Format(rsConfirmOverwriteFile, [aFile]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      if not DeleteFileUTF8(FileList[ItemIndex]) then
      begin
        ShowMessageFmt(rsErrorDeletingFile, [aFile]);
        Exit;
      end;
    end
    else // Don't delete
    begin
      Exit;
    end;
  end;

  // Creating folder
  if not DirectoryExistsUTF8(ExtractFileDir(aFile)) then
    ForceDirectoriesUTF8(ExtractFileDir(aFile));

  iImage.Picture.SaveToFile(aFile);
end;

constructor TfmEmutecaSoftImgPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmEmutecaSoftImgPreview.Destroy;
begin
  inherited Destroy;
end;

end.
