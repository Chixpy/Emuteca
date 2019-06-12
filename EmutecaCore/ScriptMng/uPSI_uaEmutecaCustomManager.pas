unit uPSI_uaEmutecaCustomManager;
{< caEmutecaCustomManager import for Pascal Script.

  This file is part of Emuteca Core.

  Copyright (C) 2018-2019 Chixpy

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
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler,
  LazFileUtils, LazUTF8,
  uEmutecaConst,
  uaEmutecaCustomManager;

type
  (*----------------------------------------------------------------------------*)
  TPSImport_uaEmutecaCustomManager = class(TPSPlugin)
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;


{ compile-time registration functions }
procedure SIRegister_caEmutecaCustomManagerTxt(CL: TPSPascalCompiler);
procedure SIRegister_caEmutecaCustomManagerIni(CL: TPSPascalCompiler);
procedure SIRegister_uaEmutecaCustomManager(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_caEmutecaCustomManagerTxt(CL: TPSRuntimeClassImporter);
procedure RIRegister_caEmutecaCustomManagerIni(CL: TPSRuntimeClassImporter);
procedure RIRegister_uaEmutecaCustomManager(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_uaEmutecaCustomManager]);
end;

procedure SIRegister_caEmutecaCustomManagerTxt(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caCHXStorableTxt', 'caEmutecaCustomManagerTxt') do
  with CL.AddClassN(CL.FindClass('caCHXStorableTxt'),
      'caEmutecaCustomManagerTxt') do
  begin
    RegisterProperty('ProgressCallBack', 'TEmutecaProgressCallBack', iptrw);

    RegisterMethod('procedure ImportFromFile(const aFilename: string)');
    RegisterMethod('procedure ImportFromStrLst(aTxtFile: TStrings)');
    RegisterMethod('procedure ExportToFile(const aFilename: string; ClearFile: boolean)');
    RegisterMethod('procedure ExportToStrLst(aTxtFile: TStrings)');
  end;
end;

procedure SIRegister_caEmutecaCustomManagerIni(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caCHXStorableIni', 'caEmutecaCustomManagerIni') do
  with CL.AddClassN(CL.FindClass('caCHXStorableIni'),
      'caEmutecaCustomManagerIni') do
  begin
    RegisterProperty('ProgressCallBack', 'TEmutecaProgressCallBack', iptrw);

    RegisterMethod('procedure ImportFromFile(const aFilename: string)');
    RegisterMethod('procedure ImportFromIni(aIniFile: TMemIniFile)');
    RegisterMethod('procedure ExportToFile(const aFilename: string; ClearFile: boolean)');
    RegisterMethod('procedure ExportToIni(aIniFile: TMemIniFile)');
  end;
end;

procedure SIRegister_uaEmutecaCustomManager(CL: TPSPascalCompiler);
begin
  SIRegister_caEmutecaCustomManagerTxt(CL);
  SIRegister_caEmutecaCustomManagerIni(CL);
end;

procedure caEmutecaCustomManagerTxtProgressCallBack_W(
  Self: caEmutecaCustomManagerTxt; const T: TEmutecaProgressCallBack);
begin
  Self.ProgressCallBack := T;
end;

procedure caEmutecaCustomManagerTxtProgressCallBack_R(
  Self: caEmutecaCustomManagerTxt; var T: TEmutecaProgressCallBack);
begin
  T := Self.ProgressCallBack;
end;

procedure caEmutecaCustomManagerIniProgressCallBack_W(
  Self: caEmutecaCustomManagerIni; const T: TEmutecaProgressCallBack);
begin
  Self.ProgressCallBack := T;
end;

procedure caEmutecaCustomManagerIniProgressCallBack_R(
  Self: caEmutecaCustomManagerIni; var T: TEmutecaProgressCallBack);
begin
  T := Self.ProgressCallBack;
end;

procedure RIRegister_caEmutecaCustomManagerTxt(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(caEmutecaCustomManagerTxt) do
  begin
    RegisterPropertyHelper(@caEmutecaCustomManagerTxtProgressCallBack_R,
      @caEmutecaCustomManagerTxtProgressCallBack_W, 'ProgressCallBack');

    RegisterVirtualMethod(@caEmutecaCustomManagerTxt.ImportFromFile,
      'ImportFromFile');
    RegisterVirtualAbstractMethod(caEmutecaCustomManagerTxt,
      @caEmutecaCustomManagerTxt.ImportFromStrLst, 'ImportFromStrLst');
    RegisterVirtualMethod(@caEmutecaCustomManagerTxt.ExportToFile,
      'ExportToFile');
    RegisterVirtualAbstractMethod(caEmutecaCustomManagerTxt,
      @caEmutecaCustomManagerTxt.ExportToStrLst, 'ExportToStrLst');
  end;
end;

procedure RIRegister_caEmutecaCustomManagerIni(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(caEmutecaCustomManagerIni) do
  begin
    RegisterPropertyHelper(@caEmutecaCustomManagerIniProgressCallBack_R,
      @caEmutecaCustomManagerIniProgressCallBack_W, 'ProgressCallBack');

    RegisterVirtualMethod(@caEmutecaCustomManagerIni.ImportFromFile,
      'ImportFromFile');
    RegisterVirtualAbstractMethod(caEmutecaCustomManagerIni,
      @caEmutecaCustomManagerIni.ImportFromIni, 'ImportFromIni');
    RegisterVirtualMethod(@caEmutecaCustomManagerIni.ExportToFile,
      'ExportToFile');
    RegisterVirtualAbstractMethod(caEmutecaCustomManagerIni,
      @caEmutecaCustomManagerIni.ExportToIni, 'ExportToIni');
  end;
end;

procedure RIRegister_uaEmutecaCustomManager(CL: TPSRuntimeClassImporter);
begin
  RIRegister_caEmutecaCustomManagerTxt(CL);
  RIRegister_caEmutecaCustomManagerIni(CL);
end;

procedure TPSImport_uaEmutecaCustomManager.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_uaEmutecaCustomManager(CompExec.comp);
end;

procedure TPSImport_uaEmutecaCustomManager.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_uaEmutecaCustomManager(ri);
end;

end.
