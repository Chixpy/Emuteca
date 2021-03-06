unit ucEmutecaScriptEngine;
{< cEmutecaScriptEngine class unit.

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
  Classes, SysUtils, uPSComponent, uPSRuntime, uPSCompiler, uPSUtils,
  // CHX classes
  ucCHXScriptEngine,
  // Emuteca
  ucEmuteca,
  // PS Imports
  uPSI_uaCHXStorable, uPSI_uaEmutecaCustomManager,
  uPSI_uEmutecaConst, uPSI_uEmutecaRscStr, uPSI_uEmutecaCommon,
  uPSI_ucEmutecaEmulator, uPSI_ucEmutecaSystem,
  uPSI_ucEmutecaGroup, uPSI_ucEmutecaSoftware,
  uPSI_ucEmutecaEmulatorManager, uPSI_ucEmutecaSystemManager,
  uPSI_ucEmutecaGroupManager, uPSI_ucEmutecaSoftManager,
  uPSI_ucEmutecaConfig, uPSI_ucEmuteca;

type

  { cEmutecaScriptEngine }

  cEmutecaScriptEngine = class(cCHXScriptEngine)
  private
    FEmuteca: cEmuteca;
    procedure SetEmuteca(AValue: cEmuteca);

  protected
    procedure PasScriptOnCompImport(Sender: TObject;
      x: TPSPascalCompiler); override;
    procedure PasScriptOnCompile(Sender: TPSScript); override;
    procedure PasScriptOnExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter); override;
    procedure PasScriptOnExecute(Sender: TPSScript); override;
    function PasScriptOnFindUnknownFile(Sender: TObject;
      const OriginFileName: tbtstring; var FileName, Output: tbtstring): boolean;
      override;
    function PasScriptOnNeedFile(Sender: TObject;
      const OriginFileName: tbtstring;
      var FileName, Output: tbtstring): boolean; override;


  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cEmutecaScriptEngine }

procedure cEmutecaScriptEngine.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;
end;

procedure cEmutecaScriptEngine.PasScriptOnCompImport(Sender: TObject;
  x: TPSPascalCompiler);
begin
  inherited PasScriptOnCompImport(Sender, x);

  SIRegister_uaCHXStorable(x);

  SIRegister_uEmutecaConst(x);
  SIRegister_uEmutecaRscStr(x);
  SIRegister_uEmutecaCommon(x);

  SIRegister_uaEmutecaCustomManager(x);
  SIRegister_ucEmutecaEmulator(x);
  SIRegister_ucEmutecaSystem(x);
  SIRegister_ucEmutecaGroup(x);
  SIRegister_ucEmutecaSoftware(x);
  SIRegister_ucEmutecaEmulatorManager(x);
  SIRegister_ucEmutecaSystemManager(x);
  SIRegister_ucEmutecaGroupManager(x);
  SIRegister_ucEmutecaSoftManager(x);
  SIRegister_ucEmutecaConfig(x);
  SIRegister_ucEmuteca(x);
end;

procedure cEmutecaScriptEngine.PasScriptOnExecImport(Sender: TObject;
  se: TPSExec; x: TPSRuntimeClassImporter);
begin
  inherited PasScriptOnExecImport(Sender, se, x);

  RIRegister_uaCHXStorable(x);

  RIRegister_uEmutecaConst_Routines(se);
  RIRegister_uEmutecaRscStr_Routines(se);
  RIRegister_uEmutecaCommon_Routines(se);

  RIRegister_uaEmutecaCustomManager(x);
  RIRegister_ucEmutecaEmulator(x);
  RIRegister_ucEmutecaSystem(x);
  RIRegister_ucEmutecaGroup(x);
  RIRegister_ucEmutecaSoftware(x);
  RIRegister_ucEmutecaEmulatorManager(x);
  RIRegister_ucEmutecaSystemManager(x);
  RIRegister_ucEmutecaGroupManager(x);
  RIRegister_ucEmutecaSoftManager(x);
  RIRegister_ucEmutecaConfig(x);
  RIRegister_ucEmuteca(x);
end;

procedure cEmutecaScriptEngine.PasScriptOnCompile(Sender: TPSScript);
begin
  inherited PasScriptOnCompile(Sender);

  // Variables
  Sender.AddRegisteredPTRVariable('Emuteca', 'cEmuteca');
end;

procedure cEmutecaScriptEngine.PasScriptOnExecute(Sender: TPSScript);
begin
  inherited PasScriptOnExecute(Sender);

  Sender.SetPointerToData('Emuteca', @FEmuteca,
    Sender.FindNamedType('cEmuteca'));
end;

function cEmutecaScriptEngine.PasScriptOnFindUnknownFile(Sender: TObject;
  const OriginFileName: tbtstring; var FileName, Output: tbtstring): boolean;
begin
  Result := inherited PasScriptOnFindUnknownFile(Sender,
    OriginFileName, FileName, Output);
end;

function cEmutecaScriptEngine.PasScriptOnNeedFile(Sender: TObject;
  const OriginFileName: tbtstring; var FileName, Output: tbtstring): boolean;
begin
  Result := inherited PasScriptOnNeedFile(Sender, OriginFileName,
    FileName, Output);
end;

constructor cEmutecaScriptEngine.Create;
begin
  inherited Create;
end;

destructor cEmutecaScriptEngine.Destroy;
begin
  inherited Destroy;
end;

end.
