unit uPSI_ucEmutecaGroupList;

{< Exports of ucEmutecaGroupList for Pascal Script engine of Emuteca.

  This file is part of Emuteca Core.

  Copyright (C) 2023 Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler,
  // Emuteca Core classes
  ucEmutecaGroup, ucEmutecaGroupList;

type

  { TPSImport_ucEmutecaGroupList }

  TPSImport_ucEmutecaGroupList = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;

procedure SIRegister_cEmutecaGroupList(CL: TPSPascalCompiler);
procedure SIRegister_ucEmutecaGroupList(CL: TPSPascalCompiler);

procedure RIRegister_cEmutecaGroupList(CL: TPSRuntimeClassImporter);
procedure RIRegister_ucEmutecaGroupList(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure SIRegister_cEmutecaGroupList(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TObject', 'cEmutecaGroupList') do
  with CL.AddClassN(CL.FindClass('TObject'), 'cEmutecaGroupList') do
  begin
    RegisterProperty('Count', 'integer', iptr);
    RegisterProperty('Items', 'cEmutecaGroup integer', iptrw);
    SetDefaultPropery('Items');

    RegisterMethod('function Add(Item: cEmutecaGroup): integer');
    RegisterMethod('procedure Delete(Index: integer)');
    RegisterMethod('function IndexOf(Item: cEmutecaGroup): integer');
  end;
end;

procedure SIRegister_ucEmutecaGroupList(CL: TPSPascalCompiler);
begin
  SIRegister_cEmutecaGroupList(CL);
end;


procedure cEmutecaGroupListCount_R(Self: cEmutecaGroupList; var T: integer);
begin
  T := Self.Count;
end;

procedure cEmutecaGroupListItems_R(Self: cEmutecaGroupList;
  var T: cEmutecaGroup; I: longint);
begin
  T := Self.Items[I];
end;

procedure cEmutecaGroupListItems_W(Self: cEmutecaGroupList;
  const T: cEmutecaGroup; I: longint);
begin
  Self.Items[I] := T;
end;

procedure RIRegister_cEmutecaGroupList(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaGroupList) do
  begin
    RegisterPropertyHelper(@cEmutecaGroupListCount_R, nil, 'Count');
    RegisterPropertyHelper(@cEmutecaGroupListItems_R,
      @cEmutecaGroupListItems_W, 'Items');

    RegisterMethod(@cEmutecaGroupList.Add, 'Add');
    RegisterMethod(@cEmutecaGroupList.Delete, 'Delete');
    RegisterMethod(@cEmutecaGroupList.IndexOf, 'IndexOf');
  end;
end;

procedure RIRegister_ucEmutecaGroupList(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaGroupList(CL);
end;

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_ucEmutecaGroupList]);
end;

{ TPSImport_ucEmutecaGroupList }

procedure TPSImport_ucEmutecaGroupList.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_ucEmutecaGroupList(CompExec.comp);
end;

procedure TPSImport_ucEmutecaGroupList.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ucEmutecaGroupList(ri);
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

