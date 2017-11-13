{ This file is part of Emuteca

  Copyright (C) 2006-2017 Chixpy

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

{ cEmutecaGroup unit. }
unit ucEmutecaGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8,
  uEmutecaCommon,
  uaEmutecaCustomSystem, uaEmutecaCustomGroup,
  ucEmutecaSoftList;

type
  { cEmutecaGroup }

  cEmutecaGroup = class(caEmutecaCustomGroup)
  private
    FCachedSystem: caEmutecaCustomSystem;
    FSoftList: cEmutecaSoftList;
    procedure SetCachedSystem(AValue: caEmutecaCustomSystem);

  public
    property SoftList: cEmutecaSoftList read FSoftList;

    property CachedSystem: caEmutecaCustomSystem
      read FCachedSystem write SetCachedSystem;


    procedure SearchAllRelatedFiles(OutFileList: TStrings;
      aFolder: string; Extensions: TStrings; AutoExtract: boolean); override;
    function SearchFirstRelatedFile(aFolder: string;
      Extensions: TStrings; AutoExtract: boolean): string; override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
  end;

  TEmutecaReturnGroupCB = function(aGroup: cEmutecaGroup): boolean of
    object;

implementation

{ cEmutecaGroup }

procedure cEmutecaGroup.SetCachedSystem(AValue: caEmutecaCustomSystem);
begin
  if FCachedSystem = AValue then
    Exit;
  FCachedSystem := AValue;
end;

procedure cEmutecaGroup.SearchAllRelatedFiles(OutFileList: TStrings;
  aFolder: string; Extensions: TStrings; AutoExtract: boolean);
begin
  if Assigned(CachedSystem) then
    EmuTKSearchAllRelatedFiles(OutFileList, aFolder, MediaFileName, Extensions,
      AutoExtract, CachedSystem.TempFolder)
  else
    inherited;
end;

function cEmutecaGroup.SearchFirstRelatedFile(aFolder: string;
  Extensions: TStrings; AutoExtract: boolean): string;
begin
   if Assigned(CachedSystem) then
    Result := EmuTKSearchFirstRelatedFile(aFolder, MediaFileName, Extensions,
      True, AutoExtract, CachedSystem.TempFolder)
  else
    Result := inherited;
end;


constructor cEmutecaGroup.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FSoftList := cEmutecaSoftList.Create(False);
end;

destructor cEmutecaGroup.Destroy;
begin
  SoftList.Free;
  inherited Destroy;
end;


initialization
  RegisterClass(cEmutecaGroup);

finalization
  UnRegisterClass(cEmutecaGroup);

end.
