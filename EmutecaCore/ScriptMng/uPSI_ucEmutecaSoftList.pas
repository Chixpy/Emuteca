unit uPSI_ucEmutecaSoftList;

{< Exports of ucEmutecaSoftList for Pascal Script engine of Emuteca.

  This file is part of Emuteca Core.

  Copyright (C) 2023 Chixpy

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
{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler,
  // Emuteca Core classes
  ucEmutecaSoftware, ucEmutecaSoftList;

type

  { TPSImport_ucEmutecaSoftList }

  TPSImport_ucEmutecaSoftList = class(TPSPlugin)
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;

procedure SIRegister_cEmutecaSoftList(CL: TPSPascalCompiler);
procedure SIRegister_ucEmutecaSoftList(CL: TPSPascalCompiler);

procedure RIRegister_cEmutecaSoftList(CL: TPSRuntimeClassImporter);
procedure RIRegister_ucEmutecaSoftList(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure SIRegister_cEmutecaSoftList(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TObject', 'cEmutecaSoftList') do
  with CL.AddClassN(CL.FindClass('TObject'), 'cEmutecaSoftList') do
  begin
    RegisterProperty('Count', 'integer', iptr);
    RegisterProperty('Items', 'cEmutecaSoftware integer', iptrw);
    SetDefaultPropery('Items');

    RegisterMethod('function Add(Item: cEmutecaSoftware): integer');
    RegisterMethod('procedure Delete(Index: integer)');
    RegisterMethod('function IndexOf(Item: cEmutecaSoftware): integer');
  end;
end;

procedure SIRegister_ucEmutecaSoftList(CL: TPSPascalCompiler);
begin
  SIRegister_cEmutecaSoftList(CL);
end;


procedure cEmutecaSoftListCount_R(Self: cEmutecaSoftList; var T: integer);
begin
  T := Self.Count;
end;

procedure cEmutecaSoftListItems_R(Self: cEmutecaSoftList;
  var T: cEmutecaSoftware; I: longint);
begin
  T := Self.Items[I];
end;

procedure cEmutecaSoftListItems_W(Self: cEmutecaSoftList;
  const T: cEmutecaSoftware; I: longint);
begin
  Self.Items[I] := T;
end;

procedure RIRegister_cEmutecaSoftList(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaSoftList) do
  begin
    RegisterPropertyHelper(@cEmutecaSoftListCount_R, nil, 'Count');
    RegisterPropertyHelper(@cEmutecaSoftListItems_R,
      @cEmutecaSoftListItems_W, 'Items');

    RegisterMethod(@cEmutecaSoftList.Add, 'Add');
    RegisterMethod(@cEmutecaSoftList.Delete, 'Delete');
    RegisterMethod(@cEmutecaSoftList.IndexOf, 'IndexOf');
  end;
end;

procedure RIRegister_ucEmutecaSoftList(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaSoftList(CL);
end;

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_ucEmutecaSoftList]);
end;

{ TPSImport_ucEmutecaSoftList }

procedure TPSImport_ucEmutecaSoftList.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_ucEmutecaSoftList(CompExec.comp);
end;

procedure TPSImport_ucEmutecaSoftList.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ucEmutecaSoftList(ri);
end;

end.

