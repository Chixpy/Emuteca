unit uPSI_uaEmutecaCustomSGItem;

{< caEmutecaCustomSGItem import for Pascal Script.

  This file is part of Emuteca Core.

  Copyright (C) 2023 Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uPSComponent, uPSRuntime, uPSCompiler,
  // CHX abstracts
  uaEmutecaCustomSGItem,
  // CHX classes
  ucEmutecaPlayingStats;

type

  { TPSImport_uaEmutecaCustomSGItem }

  TPSImport_uaEmutecaCustomSGItem = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;

procedure SIRegister_caEmutecaCustomSGItem(CL: TPSPascalCompiler);
procedure SIRegister_uaEmutecaCustomSGItem(CL: TPSPascalCompiler);

procedure RIRegister_caEmutecaCustomSGItem(CL: TPSRuntimeClassImporter);
procedure RIRegister_uaEmutecaCustomSGItem(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_uaEmutecaCustomSGItem]);
end;

procedure SIRegister_caEmutecaCustomSGItem(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caCHXStorableTxt', 'caEmutecaCustomSGItem') do
  with CL.AddClassN(CL.FindClass('caCHXStorableTxt'),
      'caEmutecaCustomSGItem') do
  begin
    RegisterProperty('ID', 'string', iptrw);
    RegisterProperty('Title', 'string', iptrw);
    RegisterProperty('SortTitle', 'string', iptrw);
    RegisterProperty('Date', 'string', iptrw);
    RegisterProperty('MediaFileName', 'string', iptr);
    RegisterProperty('Stats', 'cEmutecaPlayingStats', iptr);

    RegisterMethod('function CompareID(aID: string): integer;');
    RegisterMethod('function MatchID(aID: string): boolean;');
    RegisterMethod('function GetActualID: string;');
    RegisterMethod('function GetActualTitle: string;');
    RegisterMethod('function GetActualSortTitle: string;');
  end;
end;

procedure SIRegister_uaEmutecaCustomSGItem(CL: TPSPascalCompiler);
begin
  SIRegister_caEmutecaCustomSGItem(CL);
end;

procedure caEmutecaCustomSGItemID_R(Self: caEmutecaCustomSGItem;
  var T: string);
begin
  T := Self.ID;
end;

procedure caEmutecaCustomSGItemID_W(Self: caEmutecaCustomSGItem;
  const T: string);
begin
  Self.ID := T;
end;

procedure caEmutecaCustomSGItemTitle_R(Self: caEmutecaCustomSGItem;
  var T: string);
begin
  T := Self.Title;
end;

procedure caEmutecaCustomSGItemTitle_W(Self: caEmutecaCustomSGItem;
  const T: string);
begin
  Self.Title := T;
end;

procedure caEmutecaCustomSGItemSortTitle_R(Self: caEmutecaCustomSGItem;
  var T: string);
begin
  T := Self.SortTitle;
end;

procedure caEmutecaCustomSGItemSortTitle_W(Self: caEmutecaCustomSGItem;
  const T: string);
begin
  Self.SortTitle := T;
end;

procedure caEmutecaCustomSGItemDate_R(Self: caEmutecaCustomSGItem;
  var T: string);
begin
  T := Self.Date;
end;

procedure caEmutecaCustomSGItemDate_W(Self: caEmutecaCustomSGItem;
  const T: string);
begin
  Self.Date := T;
end;

procedure caEmutecaCustomSGItemMediaFileName_R(Self: caEmutecaCustomSGItem;
  var T: string);
begin
  T := Self.MediaFileName;
end;

procedure caEmutecaCustomSGItemStats_R(Self: caEmutecaCustomSGItem;
  var T: cEmutecaPlayingStats);
begin
  T := Self.Stats;
end;

procedure RIRegister_caEmutecaCustomSGItem(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(caEmutecaCustomSGItem) do
  begin
    RegisterPropertyHelper(@caEmutecaCustomSGItemID_R,
      @caEmutecaCustomSGItemID_W, 'ID');
    RegisterPropertyHelper(@caEmutecaCustomSGItemTitle_R,
      @caEmutecaCustomSGItemTitle_W, 'Title');
    RegisterPropertyHelper(@caEmutecaCustomSGItemSortTitle_R,
      @caEmutecaCustomSGItemSortTitle_W, 'SortTitle');
    RegisterPropertyHelper(@caEmutecaCustomSGItemDate_R,
      @caEmutecaCustomSGItemDate_W, 'Date');
    RegisterPropertyHelper(@caEmutecaCustomSGItemMediaFileName_R,
      nil, 'MediaFileName');
    RegisterPropertyHelper(@caEmutecaCustomSGItemStats_R, nil, 'Stats');

    RegisterMethod(@caEmutecaCustomSGItem.CompareID, 'CompareID');
    RegisterMethod(@caEmutecaCustomSGItem.MatchID, 'MatchID');
    RegisterMethod(@caEmutecaCustomSGItem.GetActualID, 'GetActualID');
    RegisterMethod(@caEmutecaCustomSGItem.GetActualTitle, 'GetActualTitle');
    RegisterMethod(@caEmutecaCustomSGItem.GetActualSortTitle,
      'GetActualSortTitle');
  end;
end;

procedure RIRegister_uaEmutecaCustomSGItem(CL: TPSRuntimeClassImporter);
begin
  RIRegister_caEmutecaCustomSGItem(CL);
end;

{ TPSImport_uaEmutecaCustomSGItem }

procedure TPSImport_uaEmutecaCustomSGItem.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_uaEmutecaCustomSGItem(CompExec.comp);
end;

procedure TPSImport_uaEmutecaCustomSGItem.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_uaEmutecaCustomSGItem(ri);
end;

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
