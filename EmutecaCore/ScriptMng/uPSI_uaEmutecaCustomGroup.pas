unit uPSI_uaEmutecaCustomGroup;
{< caEmutecaCustomGroup import for Pascal Script.

  Copyright (C) 2018-2019 Chixpy

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
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler, IniFiles, LazUTF8,
  uaCHXStorable, uEmutecaCommon, ucEmutecaPlayingStats,
  uaEmutecaCustomGroup;

type

  TPSImport_uaEmutecaCustomGroup = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;


{ compile-time registration functions }
procedure SIRegister_caEmutecaCustomGroup(CL: TPSPascalCompiler);
procedure SIRegister_uaEmutecaCustomGroup(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_caEmutecaCustomGroup(CL: TPSRuntimeClassImporter);
procedure RIRegister_uaEmutecaCustomGroup(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_uaEmutecaCustomGroup]);
end;


procedure SIRegister_caEmutecaCustomGroup(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caCHXStorableTxt', 'caEmutecaCustomGroup') do
  with CL.AddClassN(CL.FindClass('caCHXStorableTxt'),
      'caEmutecaCustomGroup') do
  begin
    RegisterMethod('function GetActualTitle: string;');
    RegisterMethod('function GetActualSortTitle: string;');
    RegisterMethod('function GetActualMediaFilename: string;');
    RegisterMethod('function CompareID(aID: string): integer;');
    RegisterMethod('function MatchID(aID: string): boolean;');
    RegisterMethod('procedure LoadFromStrLst(aTxtFile: TStrings); override;');
    RegisterMethod('procedure ExportToStrLst(aTxtFile: TStrings); virtual;');
    RegisterMethod('procedure SaveToStrLst(aTxtFile: TStrings); override;');
    RegisterMethod('function ExportCommaText: string;');
    RegisterMethod('procedure ImportFrom(aGroup: caEmutecaCustomGroup);');

    RegisterProperty('ID', 'string', iptrw);
    RegisterProperty('Title', 'string', iptrw);
    RegisterProperty('SortTitle', 'string', iptrw);
    RegisterProperty('Year', 'string', iptrw);
    RegisterProperty('Developer', 'string', iptrw);
    RegisterProperty('MediaFileName', 'string', iptrw);
    RegisterProperty('Stats', 'cEmutecaPlayingStats', iptr);
  end;
end;

procedure SIRegister_uaEmutecaCustomGroup(CL: TPSPascalCompiler);
begin
  SIRegister_caEmutecaCustomGroup(CL);
end;

procedure caEmutecaCustomGroupStats_R(Self: caEmutecaCustomGroup;
  var T: cEmutecaPlayingStats);
begin
  T := Self.Stats;
end;

procedure caEmutecaCustomGroupDeveloper_W(Self: caEmutecaCustomGroup;
  const T: string);
begin
  Self.Developer := T;
end;

procedure caEmutecaCustomGroupDeveloper_R(Self: caEmutecaCustomGroup;
  var T: string);
begin
  T := Self.Developer;
end;

procedure caEmutecaCustomGroupYear_W(Self: caEmutecaCustomGroup;
  const T: string);
begin
  Self.Year := T;
end;

procedure caEmutecaCustomGroupYear_R(Self: caEmutecaCustomGroup;
  var T: string);
begin
  T := Self.Year;
end;

procedure caEmutecaCustomGroupTitle_W(Self: caEmutecaCustomGroup;
  const T: string);
begin
  Self.Title := T;
end;

procedure caEmutecaCustomGroupTitle_R(Self: caEmutecaCustomGroup;
  var T: string);
begin
  T := Self.Title;
end;

procedure caEmutecaCustomGroupSortTitle_W(Self: caEmutecaCustomGroup;
  const T: string);
begin
  Self.SortTitle := T;
end;

procedure caEmutecaCustomGroupSortTitle_R(Self: caEmutecaCustomGroup;
  var T: string);
begin
  T := Self.SortTitle;
end;

procedure caEmutecaCustomGroupMediaFileName_W(Self: caEmutecaCustomGroup;
  const T: string);
begin
  Self.MediaFileName := T;
end;

procedure caEmutecaCustomGroupMediaFileName_R(Self: caEmutecaCustomGroup;
  var T: string);
begin
  T := Self.MediaFileName;
end;

procedure caEmutecaCustomGroupID_W(Self: caEmutecaCustomGroup;
  const T: string);
begin
  Self.ID := T;
end;

procedure caEmutecaCustomGroupID_R(Self: caEmutecaCustomGroup; var T: string);
begin
  T := Self.ID;
end;

procedure RIRegister_caEmutecaCustomGroup(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(caEmutecaCustomGroup) do
  begin
    RegisterMethod(@caEmutecaCustomGroup.GetActualTitle, 'GetActualTitle');
    RegisterMethod(@caEmutecaCustomGroup.GetActualSortTitle, 'GetActualSortTitle');
    RegisterMethod(@caEmutecaCustomGroup.GetActualMediaFilename, 'GetActualMediaFilename');
    RegisterMethod(@caEmutecaCustomGroup.CompareID, 'CompareID');
    RegisterMethod(@caEmutecaCustomGroup.MatchID, 'MatchID');
    RegisterVirtualMethod(@caEmutecaCustomGroup.LoadFromStrLst, 'LoadFromStrLst');
    RegisterVirtualMethod(@caEmutecaCustomGroup.ExportToStrLst, 'ExportToStrLst');
    RegisterVirtualMethod(@caEmutecaCustomGroup.SaveToStrLst, 'SaveToStrLst');
    RegisterMethod(@caEmutecaCustomGroup.ExportCommaText, 'ExportCommaText');
    RegisterMethod(@caEmutecaCustomGroup.ImportFrom, 'ImportFrom');


    RegisterPropertyHelper(@caEmutecaCustomGroupID_R, @caEmutecaCustomGroupID_W, 'ID');
    RegisterPropertyHelper(@caEmutecaCustomGroupTitle_R, @caEmutecaCustomGroupTitle_W, 'Title');
    RegisterPropertyHelper(@caEmutecaCustomGroupSortTitle_R, @caEmutecaCustomGroupSortTitle_W, 'SortTitle');
    RegisterPropertyHelper(@caEmutecaCustomGroupYear_R, @caEmutecaCustomGroupYear_W, 'Year');
    RegisterPropertyHelper(@caEmutecaCustomGroupDeveloper_R, @caEmutecaCustomGroupDeveloper_W, 'Developer');
    RegisterPropertyHelper(@caEmutecaCustomGroupMediaFileName_R, @caEmutecaCustomGroupMediaFileName_W, 'MediaFileName');
    RegisterPropertyHelper(@caEmutecaCustomGroupStats_R, nil, 'Stats');
  end;
end;

procedure RIRegister_uaEmutecaCustomGroup(CL: TPSRuntimeClassImporter);
begin
  RIRegister_caEmutecaCustomGroup(CL);
end;

procedure TPSImport_uaEmutecaCustomGroup.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_uaEmutecaCustomGroup(CompExec.comp);
end;

procedure TPSImport_uaEmutecaCustomGroup.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_uaEmutecaCustomGroup(ri);
end;

end.
