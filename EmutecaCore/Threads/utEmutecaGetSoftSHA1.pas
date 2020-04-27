unit utEmutecaGetSoftSHA1;
{< ctEmutecaGetSoftSHA1 thread unit.

  This file is part of Emuteca Core.

  Copyright (C) 2017-2020 Chixpy

  This source is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by
  writing to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sha1, LazFileUtils,
  // CHX units
  uCHX7zWrapper, uCHXStrUtils,
  // Emuteca classes
  ucEmutecaSystemManager, ucEmutecaSoftware, ucEmutecaSoftList;

type

  { ctEmutecaGetSoftSHA1

    Cache data Thread.}

  ctEmutecaGetSoftSHA1 = class(TThread)
  private
    FSystemManager: cEmutecaSystemManager;
    FTempFolder: string;
    procedure SetSystemManager(AValue: cEmutecaSystemManager);
    procedure SetTempFolder(AValue: string);

  protected
    procedure Execute; override;

  public
    property SystemManager: cEmutecaSystemManager
      read FSystemManager write SetSystemManager;
    {< Whole system manager. }
    property TempFolder: string read FTempFolder write SetTempFolder;
    {< Temp folder to extract files. }

    constructor Create;
  end;

implementation

{ ctEmutecaGetSoftSHA1 }

procedure ctEmutecaGetSoftSHA1.SetSystemManager(AValue:
  cEmutecaSystemManager);
begin
  if FSystemManager = AValue then
    Exit;
  FSystemManager := AValue;
end;

procedure ctEmutecaGetSoftSHA1.SetTempFolder(AValue: string);
begin
  FTempFolder := SetAsFolder(AValue);
end;

procedure ctEmutecaGetSoftSHA1.Execute;
var
  aSoft: cEmutecaSoftware;
  aFolder, aFile: string;
  aSha1: TSHA1Digest;
  CurrSysPos, CurrSoftPos: integer;
  SoftList: cEmutecaSoftList;
begin
  if not Assigned(SystemManager) then
    Exit;

  if TempFolder = '' then
    Exit;

  // Caching SHA1
  try
    CurrSysPos := 0;
    while (not Terminated) and (CurrSysPos < SystemManager.FullList.Count) do
    begin
      SoftList := SystemManager.FullList[CurrSysPos].SoftManager.FullList;

      CurrSoftPos := 0;
      while (not Terminated) and (CurrSoftPos < SoftList.Count) do
      begin
        aSoft := SoftList[CurrSoftPos];
        aFolder := aSoft.Folder;
        aFile := aSoft.FileName;

        if aSoft.SHA1IsEmpty then
        begin
          if DirectoryExistsUTF8(aFolder) then
          begin
            if FileExistsUTF8(aFolder + aFile) then
            begin
              aSha1 := SHA1File(aFolder + aFile);
              if not terminated then
                aSoft.SHA1 := aSha1;
            end;
          end
          else
          begin
            aSha1 := w7zSHA32InnerFile(aFolder, aFile, '');
            if not terminated then
              aSoft.SHA1 := aSha1;
          end;
        end;
        Inc(CurrSoftPos);
      end;
      Inc(CurrSysPos);
    end;
  finally
    // Catch exception if aSoft/SoftList is deleted while catching...
    // Dirty, nothing is lossed...
    ;
  end;
end;

constructor ctEmutecaGetSoftSHA1.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;


end.

