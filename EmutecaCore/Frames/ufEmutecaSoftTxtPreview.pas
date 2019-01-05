unit ufEmutecaSoftTxtPreview;

{< TfmEmutecaSoftTxtPreview frame unit.

  This file is part of Emuteca Core.

  Copyright (C) 2018-2019 Chixpy

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
  StdCtrls, ActnList, Menus, LazFileUtils,
  // CHX units
  uCHXStrUtils,
  // CHX frames
  ufCHXTxtListPreview,
  // Emuteca Core units
  uEmutecaRscStr;

type

  { TfmEmutecaSoftTxtPreview }

  TfmEmutecaSoftTxtPreview = class(TfmCHXTxtListPreview)
    actCreateNew: TAction;
    actDeleteFile: TAction;
    actSaveFile: TAction;
    actLockText: TAction;
    MenuItem1: TMenuItem;
    mipmDeleteFile: TMenuItem;
    mipmSaveFile: TMenuItem;
    mipmCreateNew: TMenuItem;
    pmTextActions: TPopupMenu;
    tbLockText: TToolButton;
    tbSaveFile: TToolButton;
    procedure actCreateNewExecute(Sender: TObject);
    procedure actDeleteFileExecute(Sender: TObject);
    procedure actLockTextExecute(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);

  private
    FSaveTextFolder: string;
    procedure SetSaveTextFolder(const aSaveTextFolder: string);

  protected
    procedure DoClearFrameData; override;

    procedure SaveTextToFile(aFile: string);

  public
    property SaveTextFolder: string read FSaveTextFolder
      write SetSaveTextFolder;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

procedure TfmEmutecaSoftTxtPreview.actLockTextExecute(Sender: TObject);
begin
  mText.ReadOnly := actLockText.Checked;
end;

procedure TfmEmutecaSoftTxtPreview.actCreateNewExecute(Sender: TObject);
var
  aFileName: string;
begin
  if SaveTextFolder = '' then
    Exit;

  // TODO: If a software is selected ask if it must be assigned to the parent
  //   or the software itself... But, how can we check it?

  // Unique filename
  aFileName := SaveTextFolder + FormatDateTime('yyyymmddhhnnss',
    Now) + '.txt';

  SaveTextToFile(aFileName);

  FileList.Add(aFileName);
end;

procedure TfmEmutecaSoftTxtPreview.actDeleteFileExecute(Sender: TObject);
begin
  if not (ItemIndex in [0..ItemCount]) then
    Exit;

  // TODO: <game>.zip/file.txt test....

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

procedure TfmEmutecaSoftTxtPreview.actSaveFileExecute(Sender: TObject);
begin
  if (ItemIndex in [0..ItemCount]) then
  begin
    // TODO: <game>.zip/file.txt test....

    mText.Lines.SaveToFile(FileList[ItemIndex]);
  end
  else
    actCreateNew.Execute;
end;

procedure TfmEmutecaSoftTxtPreview.SetSaveTextFolder(
  const aSaveTextFolder: string);
begin
  FSaveTextFolder := SetAsFolder(aSaveTextFolder);

  Enabled := SaveTextFolder <> '';
end;

procedure TfmEmutecaSoftTxtPreview.DoClearFrameData;
begin
  inherited DoClearFrameData;

  // Enabling buttons because a text can be added.
  Enabled := SaveTextFolder <> '';
end;

procedure TfmEmutecaSoftTxtPreview.SaveTextToFile(aFile: string);
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

  mText.Lines.SaveToFile(aFile);
end;

constructor TfmEmutecaSoftTxtPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  actLockText.Checked := mText.ReadOnly;
end;

destructor TfmEmutecaSoftTxtPreview.Destroy;
begin
  inherited Destroy;
end;

end.
