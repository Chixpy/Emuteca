unit uafETKGUISoftFoldersPreview;

{< TfmaETKGUISoftFoldersPreview abstract frame unit.

  This file is part of Emuteca GUI.

  Copyright (C) 2006-2023 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls,
  // CHX frames
  ufCHXFrame, ufCHXFileListPreview,
  // Emuteca Core units
  uEmutecaCommon,
  // Emuteca Core abstracts
  uaEmutecaCustomSystem,
  // Emuteca Core classes
  ucEmutecaGroup, ucEmutecaSoftware;

type

  { TfmaETKGUISoftFoldersPreview }

  TfmaETKGUISoftFoldersPreview = class(TfmCHXFrame)
    cbxFolderCaption: TComboBox;
    gbxPanel: TGroupBox;
    procedure cbxFolderCaptionSelect(Sender: TObject);

  private
    FGroup: cEmutecaGroup;
    FFileExt: TStrings;
    FFileList: TStringList;
    FLastCaption: string;
    FfmListPreview: TfmCHXFileListPreview;
    FSoftware: cEmutecaSoftware;
    FSystem: caEmutecaCustomSystem;
    FTempFolder: string;
    procedure SetFileExt(AValue: TStrings);
    procedure SetLastCaption(AValue: string);
    procedure SetSystem(AValue: caEmutecaCustomSystem);
    procedure SetTempFolder(const aTempFolder: string);

  protected
    property fmListPreview: TfmCHXFileListPreview read FfmListPreview;
    property FileList: TStringList read FFileList;

    property System: caEmutecaCustomSystem read FSystem write SetSystem;

    procedure UpdateFileList; virtual;

    procedure SetListPreview(AValue: TfmCHXFileListPreview);
    procedure SetSoftware(AValue: cEmutecaSoftware); virtual;
    procedure SetGroup(AValue: cEmutecaGroup); virtual;


    procedure CreateListView; virtual; // abstract; -> AbstractError?
    function GetCaptionList: TStrings; virtual; // abstract; -> AbstractError?
    function GetFolder: string; virtual; // abstract; -> AbstractError?

  public
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    property Group: cEmutecaGroup read FGroup write SetGroup;

    property TempFolder: string read FTempFolder write SetTempFolder;

    property FileExt: TStrings read FFileExt write SetFileExt;

    property LastCaption: string read FLastCaption write SetLastCaption;

    procedure ClearFrameData; override;
    procedure LoadFrameData;  override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TfmaETKGUISoftFoldersPreviewClass = class of TfmaETKGUISoftFoldersPreview;

implementation

{$R *.lfm}

{ TfmaETKGUISoftFoldersPreview }

procedure TfmaETKGUISoftFoldersPreview.cbxFolderCaptionSelect(Sender: TObject);
begin
  if cbxFolderCaption.ItemIndex >= 0 then
    LastCaption := cbxFolderCaption.Items[cbxFolderCaption.ItemIndex];

  UpdateFileList;
end;

procedure TfmaETKGUISoftFoldersPreview.SetGroup(AValue: cEmutecaGroup);
begin
  if FGroup = AValue then
    Exit;
  FGroup := AValue;

  FSoftware := nil;

  if Assigned(Group) then
    System := Group.CachedSystem;

  UpdateFileList;
end;

procedure TfmaETKGUISoftFoldersPreview.SetFileExt(AValue: TStrings);
begin
  if FFileExt = AValue then
    Exit;
  FFileExt := AValue;

  UpdateFileList;
end;

procedure TfmaETKGUISoftFoldersPreview.SetLastCaption(AValue: string);
begin
  if FLastCaption = AValue then
    Exit;
  FLastCaption := AValue;
end;

procedure TfmaETKGUISoftFoldersPreview.SetListPreview(AValue:
  TfmCHXFileListPreview);
begin
  if FfmListPreview = AValue then
    Exit;
  FfmListPreview := AValue;
end;

procedure TfmaETKGUISoftFoldersPreview.CreateListView;
begin
  // This method must be overrided, I don't know why it can't be abstract
end;

function TfmaETKGUISoftFoldersPreview.GetCaptionList: TStrings;
begin
  // This method must be overrided, I don't know why it can't be abstract
  Result := nil;
end;

function TfmaETKGUISoftFoldersPreview.GetFolder: string;
begin
  // This method must be overrided, I don't know why it can't be abstract
  Result := '';
end;

procedure TfmaETKGUISoftFoldersPreview.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;

  FGroup := nil;
  if Assigned(Software) then
    System := Software.CachedSystem;

  UpdateFileList;
end;

procedure TfmaETKGUISoftFoldersPreview.SetSystem(
  AValue: caEmutecaCustomSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  LoadFrameData;
end;

procedure TfmaETKGUISoftFoldersPreview.SetTempFolder(
  const aTempFolder: string);
begin
  if FTempFolder = aTempFolder then
    Exit;
  FTempFolder := aTempFolder;
end;

procedure TfmaETKGUISoftFoldersPreview.UpdateFileList;
begin
  if cbxFolderCaption.ItemIndex < 0 then
    Exit;

  fmListPreview.FileList := nil;
  FileList.Clear;

  // TODO: Emuteca Core must do this if we want to do it configurable
  // TODO ZipMedia: Make configurable search media in zip (default off)
  if Assigned(Software) then
  begin
    EmuTKSearchAllRelatedFiles(FileList, GetFolder, Software.MediaFileName,
      FileExt, False, True, TempFolder);

    if (FileList.Count = 0) and (not Software.MatchGroupFile) then
    begin
      if Assigned(Software.CachedGroup) then
      // TODO ZipMedia: Make configurable search media in zip (default off)
        EmuTKSearchAllRelatedFiles(FileList, GetFolder,
          Software.CachedGroup.MediaFileName, FileExt, False, True, TempFolder);
    end;
  end
  else if Assigned(Group) then
  begin
    // TODO ZipMedia: Make configurable search media in zip (default off)
  EmuTKSearchAllRelatedFiles(FileList, GetFolder, Group.MediaFileName,
      FileExt, False, True, TempFolder);
  end;

  fmListPreview.FileList := FileList;
end;

procedure TfmaETKGUISoftFoldersPreview.ClearFrameData;
begin
  inherited ClearFrameData;

  cbxFolderCaption.Clear;
end;

procedure TfmaETKGUISoftFoldersPreview.LoadFrameData;
var
  aIndex: integer;
begin
  inherited LoadFrameData;

  Enabled := Assigned(System);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  if System.ImageCaptions.Count > 0 then
  begin
    cbxFolderCaption.Items.Assign(GetCaptionList);

    // Restoring last caption
    aIndex := cbxFolderCaption.Items.IndexOf(LastCaption);
    if aIndex <> -1 then
      cbxFolderCaption.ItemIndex := aIndex
    else
      cbxFolderCaption.ItemIndex := 0;
  end
  else
    ClearFrameData;
end;

constructor TfmaETKGUISoftFoldersPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FFileList := TStringList.Create;

  CreateListView;
end;

destructor TfmaETKGUISoftFoldersPreview.Destroy;
begin
  FileList.Free;

  inherited Destroy;
end;

initialization
  RegisterClass(TfmaETKGUISoftFoldersPreview);

finalization
  UnRegisterClass(TfmaETKGUISoftFoldersPreview);

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
