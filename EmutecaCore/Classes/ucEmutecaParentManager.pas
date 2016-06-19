{ This file is part of Emuteca

  Copyright (C) 2006-2016 Chixpy

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

{ cGameManager unit. }
unit ucEmutecaParentManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils,
  LazUTF8, LConvEncoding,
  LResources,
  // Emuteca core
  uaEmutecaManager, ucEmutecaParent,
  // Utils
  u7zWrapper;

resourcestring
  rsLoadingParentList = 'Loading parent list...';
  rsSavingParentList = 'Saving parent list...';

type
  { cEmutecaParentManager }

  cEmutecaParentManager = class(caEmutecaManagerTxt)
  private
    FFullList: cEmutecaParentList;

  protected


  public
    property FullList: cEmutecaParentList read FFullList;
    {< Actual list where the parents are stored. }

    procedure LoadFromFileTxt(TxtFile: TStrings); override;
    procedure SaveToFileTxt(TxtFile: TStrings; const ExportMode: boolean);
      override;

    function ItemById(aId: string): cEmutecaParent;
    {< Returns the parent with aId key.

       @Result cEmutecaParent found or nil.
    }

    procedure AssingAllTo(aList: TStrings); override;
    procedure AssingEnabledTo(aList: TStrings); override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ cEmutecaParentManager }

function cEmutecaParentManager.ItemById(aId: string): cEmutecaParent;
var
  i: integer;
begin
  Result := nil;

  i := 0;
  while (Result = nil) and (i < FullList.Count) do
  begin
    if UTF8CompareText(FullList[i].ID, aId) = 0 then
      Result := FullList[i];
    inc(i);
  end;
end;

procedure cEmutecaParentManager.AssingAllTo(aList: TStrings);
var
  i: longint;
begin
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  i := 0;
  while i < FullList.Count do
  begin
    aList.AddObject(FullList[i].Title, FullList[i]);
    Inc(i);
  end;
  aList.EndUpdate;
end;

procedure cEmutecaParentManager.AssingEnabledTo(aList: TStrings);
begin
  { TODO : Maybe search for enabled systems... }
  AssingAllTo(aList);
end;

procedure cEmutecaParentManager.LoadFromFileTxt(TxtFile: TStrings);
var
  i: integer;
  TempParent: cEmutecaParent;
begin
  if not Assigned(TxtFile) then
    Exit;

  i := 1; // Skipping Header
  while i < TxtFile.Count do
  begin
    TempParent := cEmutecaParent.Create(nil);
    TempParent.DataString := TxtFile[i];
    FullList.Add(TempParent);
    Inc(i);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsLoadingParentList, TempParent.System,
        TempParent.Title, i, TxtFile.Count);
  end;
end;

procedure cEmutecaParentManager.SaveToFileTxt(TxtFile: TStrings;
  const ExportMode: boolean);
var
  i: integer;
begin
  if not Assigned(TxtFile) then
    Exit;

  if not ExportMode then
  begin
    TxtFile.Clear;
    TxtFile.Add('"ID/Sort Name","System","Title"');
  end;

  i := 0;
  while i < FullList.Count do
  begin
    TxtFile.Add(FullList[i].DataString);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsSavingParentList, FullList[i].System,
        FullList[i].Title, i + 1, FullList.Count);
    Inc(i);
  end;
end;

{ TODO : This goes with... cVersion or cEmuteca }
{function cEmutecaParentManager.Execute(aGame: cEmutecaGameVersion): integer;
var
  RomFile, CompressedFile: string;
  Error: integer;
  Compressed: boolean;
  NewDir: boolean;
  TempTime: TTime;
  aFolder: string;
begin
  // Uhm. If things go bad from the begin, they only can improve :-D
  Result := kEmutecaExecErrorNoGame;

  // No, they don't :-|
  if aGame = nil then
    Exit;
  if Emulator = nil then
    Exit;
  if System = nil then
    Exit;

  // Testing if it's in a compressed archive.
  // I don't test extensions... only if the "game's folder" is actually a
  //   file and not a folder.
  CompressedFile := ExcludeTrailingPathDelimiter(aGame.Folder);
  Compressed := FileExistsUTF8(CompressedFile);

  if Trim(ExcludeTrailingPathDelimiter(System.TempFolder)) <> '' then
    aFolder := SetAsFolder(System.TempFolder)
  else
    aFolder := SetAsFolder(TempFolder);
  aFolder := SetAsFolder(aFolder + krsEmutecaGameSubFolder);

  NewDir := not DirectoryExists(aFolder);
  if NewDir then
    ForceDirectoriesUTF8(aFolder);

  if Compressed then
  begin
    if System.ExtractAll then
      Error := w7zExtractFile(CompressedFile, AllFilesMask, aFolder, True, '')
    else
      Error := w7zExtractFile(CompressedFile, aGame.FileName,
        aFolder, True, '');
    if Error <> 0 then
      Exit;
    RomFile := aFolder + aGame.FileName;
  end
  else
  begin
    RomFile := SetAsFolder(aGame.Folder) + aGame.FileName;
    if (System.ExtractAll) and
      (CompressedExt.IndexOf(UTF8Copy(ExtractFileExt(aGame.FileName),
      2, MaxInt)) <> -1) then
    begin // The ROM is a compressed file but must be extracted anyways
      Error := w7zExtractFile(RomFile, '*', aFolder, True, '');
      if Error <> 0 then
        Exit;
      Compressed := True;
    end;
  end;

  // Last test if extracting goes wrong...
  if (RomFile = '') or not FileExistsUTF8(RomFile) then
    // Error code already set...
    Exit;

  TempTime := Now;

  Result := Emulator.Execute(RomFile);

  // if Emulator returns no error and passed at least one minute...
  if (Result = 0) and (Now > (EncodeTime(0, 1, 0, 0) + TempTime)) then
  begin
    aGame.Statistics.AddPlayingTime(Now, TempTime);
    aGame.Statistics.LastTime := TempTime;
    aGame.Statistics.TimesPlayed := aGame.Statistics.TimesPlayed + 1;
  end;

  // Kill all them
  if Compressed then
  begin
    if System.ExtractAll then
    begin
      DeleteDirectory(aFolder, False);
      if not NewDir then
        ForceDirectoriesUTF8(aFolder);
    end
    else
    begin
      if FileExistsUTF8(RomFile) then
        DeleteFileUTF8(RomFile);
    end;
  end;
end;}

constructor cEmutecaParentManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FFullList := cEmutecaParentList.Create(True);
  // TODO: OnCompare FullList.OnCompare := ;
end;

destructor cEmutecaParentManager.Destroy;
begin
  FreeAndNil(FFullList);
  inherited Destroy;
end;

end.