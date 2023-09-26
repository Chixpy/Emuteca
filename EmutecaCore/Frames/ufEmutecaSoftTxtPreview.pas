unit ufEmutecaSoftTxtPreview;

{< TfmEmutecaSoftTxtPreview frame unit.

  This file is part of Emuteca Core.

  Copyright (C) 2018-2023 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ActnList, Menus, LazFileUtils,
  // CHX units
  uCHXRscStr, uCHXStrUtils,
  // CHX frames
  ufCHXTxtListPreview,
  // Emuteca Core units
  uEmutecaRscStr;

type

  { TfmEmutecaSoftTxtPreview }

  TfmEmutecaSoftTxtPreview = class(TfmCHXTxtListPreview)
    actCreateNew: TAction;
    actSaveFile: TAction;
    actLockText: TAction;
    mipmSaveFile: TMenuItem;
    mipmCreateNew: TMenuItem;
    pmTextActions: TPopupMenu;
    tbLockText: TToolButton;
    tbSaveFile: TToolButton;
    procedure actCreateNewExecute(Sender: TObject);
    procedure actLockTextExecute(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);

  private
    FSaveTextFolder: string;
    procedure SetSaveTextFolder(const aSaveTextFolder: string);

  protected
    procedure SaveTextToFile(aFile: string);

  public
    property SaveTextFolder: string read FSaveTextFolder
      write SetSaveTextFolder;

    procedure ClearFrameData; override;

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

procedure TfmEmutecaSoftTxtPreview.actSaveFileExecute(Sender: TObject);
begin
  if (ItemIndex in [0..ItemCount - 1]) then
  begin
    SaveTextToFile(FileList[ItemIndex]);
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

procedure TfmEmutecaSoftTxtPreview.ClearFrameData;
begin
  inherited ClearFrameData;

  // Enabling buttons because a text can be added.
  Enabled := SaveTextFolder <> '';
end;

procedure TfmEmutecaSoftTxtPreview.SaveTextToFile(aFile: string);
var
  aSL: TStringList;
begin
  // Checking if file already exists...
  if FileExistsUTF8(aFile) then
  begin
    if MessageDlg(Format(rsConfirmOverwriteFile, [aFile]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      if not DeleteFileUTF8(aFile) then
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

  // HACK: Fixing a windows "feature/bug" when WordWrap is activated in TMemo.
  //   Windows actually adds no wanted linebreaks in the string lines and then
  //   are saved when mText.Lines.SaveToFile(aFile) is used.
  aSL := TStringList.Create;
  aSL.Text := mText.Text;
  aSL.SaveToFile(aFile);
  aSL.Free;
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

initialization
  RegisterClass(TfmEmutecaSoftTxtPreview);

finalization
  UnRegisterClass(TfmEmutecaSoftTxtPreview);
end.
{
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
