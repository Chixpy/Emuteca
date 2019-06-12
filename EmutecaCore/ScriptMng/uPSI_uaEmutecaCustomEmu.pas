unit uPSI_uaEmutecaCustomEmu;
{< caEmutecaCustomGroup import for Pascal Script.

  Copyright (C) 2019-2019 Chixpy

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
  Classes, SysUtils, uPSComponent, uPSRuntime, uPSCompiler,
  uaEmutecaCustomEmu;

 type

  { TPSImport_uaEmutecaCustomEmu }

  TPSImport_uaEmutecaCustomEmu = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;


{ compile-time registration functions }
procedure SIRegister_caEmutecaCustomEmu(CL: TPSPascalCompiler);
procedure SIRegister_uaEmutecaCustomEmu(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_caEmutecaCustomEmu(CL: TPSRuntimeClassImporter);
procedure RIRegister_uaEmutecaCustomEmu(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure SIRegister_caEmutecaCustomEmu(CL: TPSPascalCompiler);
begin
    //with RegClassS(CL,'caCHXStorableIni', 'caEmutecaCustomGroup') do
  with CL.AddClassN(CL.FindClass('caCHXStorableIni'),
      'caEmutecaCustomEmu') do
  begin

  end;
end;

procedure SIRegister_uaEmutecaCustomEmu(CL: TPSPascalCompiler);
begin
  SIRegister_caEmutecaCustomEmu(CL);
end;

procedure RIRegister_caEmutecaCustomEmu(CL: TPSRuntimeClassImporter);
begin
    with CL.Add(caEmutecaCustomEmu) do
  begin

  end;
end;

procedure RIRegister_uaEmutecaCustomEmu(CL: TPSRuntimeClassImporter);
begin
  RIRegister_caEmutecaCustomEmu(CL);
end;

procedure Register;
begin
   RegisterComponents('Pascal Script', [TPSImport_uaEmutecaCustomEmu]);
end;

{ TPSImport_uaEmutecaCustomEmu }

procedure TPSImport_uaEmutecaCustomEmu.CompileImport1(CompExec: TPSScript);
begin
   SIRegister_uaEmutecaCustomGroup(CompExec.comp);
end;

procedure TPSImport_uaEmutecaCustomEmu.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
   RIRegister_uaEmutecaCustomGroup(ri);
end;

end.

