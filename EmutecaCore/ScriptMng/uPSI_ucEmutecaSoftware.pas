unit uPSI_ucEmutecaSoftware;
{
This file has been generated by UnitParser v0.7, written by M. Knight
and updated by NP. v/d Spek and George Birbilis. 
Source Code from Carlo Kok has been used to implement various sections of
UnitParser. Components of ROPS are used in the construction of UnitParser,
code implementing the class wrapper is taken from Carlo Kok's conv utility

}
interface
 
uses
   SysUtils
  ,Classes
  ,uPSComponent
  ,uPSRuntime
  ,uPSCompiler
  ;
 
type 
(*----------------------------------------------------------------------------*)
  TPSImport_ucEmutecaSoftware = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;
 
 
{ compile-time registration functions }
procedure SIRegister_cEmutecaSoftware(CL: TPSPascalCompiler);
procedure SIRegister_ucEmutecaSoftware(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_cEmutecaSoftware(CL: TPSRuntimeClassImporter);
procedure RIRegister_ucEmutecaSoftware(CL: TPSRuntimeClassImporter);

procedure Register;

implementation


uses
   LazFileUtils
  ,LazUTF8
  ,uaEmutecaCustomSystem
  ,uaEmutecaCustomGroup
  ,uaEmutecaCustomSoft
  ,uEmutecaCommon
  ,uCHXStrUtils
  ,ucEmutecaSoftware
  ;
 
 
procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_ucEmutecaSoftware]);
end;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_cEmutecaSoftware(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caEmutecaCustomSoft', 'cEmutecaSoftware') do
  with CL.AddClassN(CL.FindClass('caEmutecaCustomSoft'),'cEmutecaSoftware') do
  begin
    RegisterProperty('CachedSystem', 'caEmutecaCustomSystem', iptrw);
    RegisterProperty('CachedGroup', 'caEmutecaCustomGroup', iptrw);
    RegisterMethod('Procedure FPOObservedChanged( ASender : TObject; Operation : TFPObservedOperation; Data : Pointer)');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_ucEmutecaSoftware(CL: TPSPascalCompiler);
begin
  SIRegister_cEmutecaSoftware(CL);
  CL.AddTypeS('TEmutecaReturnSoftCB', 'Function ( aSoft : cEmutecaSoftware) : b'
   +'oolean');
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure cEmutecaSoftwareCachedGroup_W(Self: cEmutecaSoftware; const T: caEmutecaCustomGroup);
begin Self.CachedGroup := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSoftwareCachedGroup_R(Self: cEmutecaSoftware; var T: caEmutecaCustomGroup);
begin T := Self.CachedGroup; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSoftwareCachedSystem_W(Self: cEmutecaSoftware; const T: caEmutecaCustomSystem);
begin Self.CachedSystem := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSoftwareCachedSystem_R(Self: cEmutecaSoftware; var T: caEmutecaCustomSystem);
begin T := Self.CachedSystem; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_cEmutecaSoftware(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaSoftware) do
  begin
    RegisterPropertyHelper(@cEmutecaSoftwareCachedSystem_R,@cEmutecaSoftwareCachedSystem_W,'CachedSystem');
    RegisterPropertyHelper(@cEmutecaSoftwareCachedGroup_R,@cEmutecaSoftwareCachedGroup_W,'CachedGroup');
    RegisterMethod(@cEmutecaSoftware.FPOObservedChanged, 'FPOObservedChanged');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_ucEmutecaSoftware(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaSoftware(CL);
end;

 
 
{ TPSImport_ucEmutecaSoftware }
(*----------------------------------------------------------------------------*)
procedure TPSImport_ucEmutecaSoftware.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_ucEmutecaSoftware(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_ucEmutecaSoftware.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ucEmutecaSoftware(ri);
end;
(*----------------------------------------------------------------------------*)
 
 
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
