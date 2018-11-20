unit uafETKGUISoftFoldersPreview;

{< TfmaETKGUISoftFoldersPreview abstract frame unit.

  This file is part of Emuteca GUI.

  Copyright (C) 2006-2018 Chixpy

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  // CHX frames
  ufCHXFrame, ufCHXStrLstPreview,
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
    FfmListPreview: TfmCHXStrLstPreview;
    FSoftware: cEmutecaSoftware;
    FSystem: caEmutecaCustomSystem;
    FTempFolder: string;
    procedure SetFileExt(AValue: TStrings);
    procedure SetLastCaption(AValue: string);
    procedure SetSystem(AValue: caEmutecaCustomSystem);
    procedure SetTempFolder(const aTempFolder: string);

  protected
    property fmListPreview: TfmCHXStrLstPreview read FfmListPreview;
    property FileList: TStringList read FFileList;

    property System: caEmutecaCustomSystem read FSystem write SetSystem;

    procedure UpdateFileList;

    procedure SetListPreview(AValue: TfmCHXStrLstPreview);
    procedure SetSoftware(AValue: cEmutecaSoftware); virtual;
    procedure SetGroup(AValue: cEmutecaGroup); virtual;

    procedure DoClearFrameData;
    procedure DoLoadFrameData;

    procedure CreateListView; virtual; // TODO: abstract; //-> ERROR?
    function GetCaptionList: TStrings; virtual; // TODO: abstract; //-> ERROR?
    function GetFolder: string; virtual; // TODO: abstract; //-> ERROR?

  public
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    property Group: cEmutecaGroup read FGroup write SetGroup;

    property TempFolder: string read FTempFolder write SetTempFolder;

    property FileExt: TStrings read FFileExt write SetFileExt;

    property LastCaption: string read FLastCaption write SetLastCaption;

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
  TfmCHXStrLstPreview);
begin
  if FfmListPreview = AValue then
    Exit;
  FfmListPreview := AValue;
end;

procedure TfmaETKGUISoftFoldersPreview.CreateListView;
begin
  // This method must be overrided
end;

function TfmaETKGUISoftFoldersPreview.GetCaptionList: TStrings;
begin
  // This method must be overrided
  Result := nil;
end;

function TfmaETKGUISoftFoldersPreview.GetFolder: string;
begin
  // This method must be overrided
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

  fmListPreview.StrList := nil;
  FileList.Clear;

  // TODO: Maybe Emuteca Core must do this...
  if Assigned(Software) then
  begin
    EmuTKSearchAllRelatedFiles(FileList, GetFolder, Software.GetMediaFileName,
      FileExt, True, True, TempFolder);

    if (FileList.Count = 0) and (not Software.MatchGroupFile) then
    begin
      if Assigned(Software.CachedGroup) then
        EmuTKSearchAllRelatedFiles(FileList, GetFolder,
          Software.CachedGroup.MediaFileName, FileExt, True, True, TempFolder);
    end;
  end
  else if Assigned(Group) then
  begin
    EmuTKSearchAllRelatedFiles(FileList, GetFolder, Group.MediaFileName,
      FileExt, True, True, TempFolder);
  end;

  fmListPreview.StrList := FileList;
end;

procedure TfmaETKGUISoftFoldersPreview.DoClearFrameData;
begin
  cbxFolderCaption.Clear;
end;

procedure TfmaETKGUISoftFoldersPreview.DoLoadFrameData;
var
  aIndex: integer;
begin
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

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
end;

destructor TfmaETKGUISoftFoldersPreview.Destroy;
begin
  FileList.Free;

  inherited Destroy;
end;

end.
