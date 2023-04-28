unit uPSI_uEmutecaGUIDialogs;

{< uPSI_uEmutecaGUIDialogs.

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
  SysUtils, Classes, Controls, uPSComponent, uPSRuntime, uPSCompiler,
  // CHX frames
  ufCHXFrame,
  // Emuteca Core units
  uEmutecaCommon,
  // Emuteca Core abstracts
  uaEmutecaCustomSGItem,
  // Emuteca Core classes
  ucEmutecaGroup, ucEmutecaSoftware, ucEmutecaSystem, ucEmutecaSystemList,
  // Emuteca GUI frames
  ufEmutecaGroupEditor, ufETKGUIFullSoftEditor, ufETKGUIIcnSysCBX,
  // Emuteca GUI PS frames
  fETKGUICompareSG;

procedure SIRegister_uEmutecaGUIDialogs(CL: TPSPascalCompiler);
procedure RIRegister_uEmutecaGUIDialogs_Routines(S: TPSExec);

implementation

procedure SIRegister_uEmutecaGUIDialogs(CL: TPSPascalCompiler);
begin
  CL.AddDelphiFunction(
    'function ETKAskSystem(aSystemList: cEmutecaSystemList): cEmutecaSystem;');

  CL.AddDelphiFunction('function ETKEditGroup(aGroup: cEmutecaGroup): integer;');
  CL.AddDelphiFunction('function ETKEditSoft(aSoft: cEmutecaSoftware): integer;');
  CL.AddDelphiFunction('function ETKCompareSG(aSG1, aSG2: caEmutecaCustomSGItem): integer;');
end;

function ETKAskSystem(aSystemList: cEmutecaSystemList): cEmutecaSystem;
begin
  Result := nil;
  if not assigned(aSystemList) then Exit;

  Result := TfmETKGUIIcnSysCBX.SimpleDialog(aSystemList);
end;

function ETKEditGroup(aGroup: cEmutecaGroup): integer;
begin
  Result := mrNone;
  if not assigned(aGroup) then Exit;

  // Result := TfmEmutecaGroupEditor.SimpleModalForm(aGroup, '', aGUIConfigIni, aGUIIconsIni);
  Result := TfmEmutecaGroupEditor.SimpleModalForm(aGroup, '', '', '');
end;

function ETKEditSoft(aSoft: cEmutecaSoftware): integer;
begin
  Result := mrNone;
  if not assigned(aSoft) then Exit;

  // Result := TfmETKGUIFullSoftEditor.SimpleModalForm(aSoft, '', aGUIConfigIni, aGUIIconsIni);
  Result := TfmETKGUIFullSoftEditor.SimpleModalForm(aSoft, '', '', '');
end;

function ETKCompareSG(aSG1, aSG2: caEmutecaCustomSGItem): integer;
begin
  Result := mrNone;
  if (not assigned(aSG1)) or (not assigned(aSG2)) then Exit;

  // Result := TfmETKGUICompareSG.SimpleModalForm(aSG1, aSG2, aGUIConfigIni, aGUIIconsIni);
  Result := TfmETKGUICompareSG.SimpleModalForm(aSG1, aSG2, '', '');
end;

procedure RIRegister_uEmutecaGUIDialogs_Routines(S: TPSExec);
begin
  S.RegisterDelphiFunction(@ETKAskSystem, 'ETKAskSystem', cdRegister);

  S.RegisterDelphiFunction(@ETKEditGroup, 'ETKEditGroup', cdRegister);
  S.RegisterDelphiFunction(@ETKEditSoft, 'ETKEditSoft', cdRegister);
  S.RegisterDelphiFunction(@ETKCompareSG, 'ETKCompareSG', cdRegister);
end;

end.
