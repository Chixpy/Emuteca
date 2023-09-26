unit uPSI_ucEmutecaSystemList;

{< Exports of ucEmutecaSystemList for Pascal Script engine of Emuteca.

  This file is part of Emuteca Core.

  Copyright (C) 2023 Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler,
  // Emuteca Core classes
  ucEmutecaSystem, ucEmutecaSystemList;

type

  { TPSImport_ucEmutecaSystemList }

  TPSImport_ucEmutecaSystemList = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;

procedure SIRegister_cEmutecaSystemList(CL: TPSPascalCompiler);
procedure SIRegister_ucEmutecaSystemList(CL: TPSPascalCompiler);

procedure RIRegister_cEmutecaSystemList(CL: TPSRuntimeClassImporter);
procedure RIRegister_ucEmutecaSystemList(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure SIRegister_cEmutecaSystemList(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TObject', 'cEmutecaSystemList') do
  with CL.AddClassN(CL.FindClass('TObject'), 'cEmutecaSystemList') do
  begin
    RegisterProperty('Count', 'integer', iptr);
    RegisterProperty('Items', 'cEmutecaSystem integer', iptrw);
    SetDefaultPropery('Items');

    RegisterMethod('function Add(Item: cEmutecaSystem): integer');
    RegisterMethod('procedure Delete(Index: integer)');
    RegisterMethod('function IndexOf(Item: cEmutecaSystem): integer');
  end;
end;

procedure SIRegister_ucEmutecaSystemList(CL: TPSPascalCompiler);
begin
  SIRegister_cEmutecaSystemList(CL);
end;


procedure cEmutecaSystemListCount_R(Self: cEmutecaSystemList; var T: integer);
begin
  T := Self.Count;
end;

procedure cEmutecaSystemListItems_R(Self: cEmutecaSystemList;
  var T: cEmutecaSystem; I: longint);
begin
  T := Self.Items[I];
end;

procedure cEmutecaSystemListItems_W(Self: cEmutecaSystemList;
  const T: cEmutecaSystem; I: longint);
begin
  Self.Items[I] := T;
end;

procedure RIRegister_cEmutecaSystemList(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaSystemList) do
  begin
    RegisterPropertyHelper(@cEmutecaSystemListCount_R, nil, 'Count');
    RegisterPropertyHelper(@cEmutecaSystemListItems_R,
      @cEmutecaSystemListItems_W, 'Items');

    RegisterMethod(@cEmutecaSystemList.Add, 'Add');
    RegisterMethod(@cEmutecaSystemList.Delete, 'Delete');
    RegisterMethod(@cEmutecaSystemList.IndexOf, 'IndexOf');
  end;
end;

procedure RIRegister_ucEmutecaSystemList(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaSystemList(CL);
end;

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_ucEmutecaSystemList]);
end;

{ TPSImport_ucEmutecaSystemList }

procedure TPSImport_ucEmutecaSystemList.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_ucEmutecaSystemList(CompExec.comp);
end;

procedure TPSImport_ucEmutecaSystemList.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ucEmutecaSystemList(ri);
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
