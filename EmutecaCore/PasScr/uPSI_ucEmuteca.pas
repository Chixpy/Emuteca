unit uPSI_ucEmuteca;
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
  TPSImport_ucEmuteca = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;
 
 
{ compile-time registration functions }
procedure SIRegister_cEmuteca(CL: TPSPascalCompiler);
procedure SIRegister_ucEmuteca(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_cEmuteca(CL: TPSRuntimeClassImporter);
procedure RIRegister_ucEmuteca(CL: TPSRuntimeClassImporter);

procedure Register;

implementation


uses
   FileUtil
  ,LazUTF8
  ,LazFileUtils
  ,u7zWrapper
  ,uCHXStrUtils
  ,uEmutecaCommon
  ,ucEmutecaConfig
  ,ucEmutecaEmulatorManager
  ,ucEmutecaSystemManager
  ,ucEmutecaParentManager
  ,ucEmutecaSoftManager
  ,ucEmutecaSoftware
  ,ucEmutecaParent
  ,ucEmutecaSystem
  ,ucEmutecaEmulator
  ,ucEmuteca
  ;
 
 
procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_ucEmuteca]);
end;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_cEmuteca(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TComponent', 'cEmuteca') do
  with CL.AddClassN(CL.FindClass('TComponent'),'cEmuteca') do
  begin
    RegisterProperty('ProgressCallBack', 'TEmutecaProgressCallBack', iptrw);
    RegisterProperty('TempFolder', 'string', iptrw);
    RegisterMethod('Procedure LoadConfig( aFile : string)');
    RegisterMethod('Function SearchParent( aID : string) : cEmutecaParent');
    RegisterMethod('Function SearchSystem( aID : string) : cEmutecaSystem');
    RegisterMethod('Function SearchMainEmulator( aID : string) : cEmutecaEmulator');
    RegisterMethod('Function RunSoftware( const aSoftware : cEmutecaSoftware) : integer');
    RegisterProperty('Config', 'cEmutecaConfig', iptrw);
    RegisterProperty('SoftManager', 'cEmutecaSoftManager', iptr);
    RegisterProperty('ParentManager', 'cEmutecaParentManager', iptr);
    RegisterProperty('EmulatorManager', 'cEmutecaEmulatorManager', iptr);
    RegisterProperty('SystemManager', 'cEmutecaSystemManager', iptr);
    RegisterProperty('CurrentSoft', 'cEmutecaSoftware', iptrw);
    RegisterProperty('CurrentParent', 'cEmutecaParent', iptrw);
    RegisterProperty('CurrentEmulator', 'cEmutecaEmulator', iptrw);
    RegisterProperty('CurrentSystem', 'cEmutecaSystem', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_ucEmuteca(CL: TPSPascalCompiler);
begin
  SIRegister_cEmuteca(CL);
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure cEmutecaCurrentSystem_W(Self: cEmuteca; const T: cEmutecaSystem);
begin Self.CurrentSystem := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaCurrentSystem_R(Self: cEmuteca; var T: cEmutecaSystem);
begin T := Self.CurrentSystem; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaCurrentEmulator_W(Self: cEmuteca; const T: cEmutecaEmulator);
begin Self.CurrentEmulator := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaCurrentEmulator_R(Self: cEmuteca; var T: cEmutecaEmulator);
begin T := Self.CurrentEmulator; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaCurrentParent_W(Self: cEmuteca; const T: cEmutecaParent);
begin Self.CurrentParent := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaCurrentParent_R(Self: cEmuteca; var T: cEmutecaParent);
begin T := Self.CurrentParent; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaCurrentSoft_W(Self: cEmuteca; const T: cEmutecaSoftware);
begin Self.CurrentSoft := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaCurrentSoft_R(Self: cEmuteca; var T: cEmutecaSoftware);
begin T := Self.CurrentSoft; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemManager_R(Self: cEmuteca; var T: cEmutecaSystemManager);
begin T := Self.SystemManager; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaEmulatorManager_R(Self: cEmuteca; var T: cEmutecaEmulatorManager);
begin T := Self.EmulatorManager; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaParentManager_R(Self: cEmuteca; var T: cEmutecaParentManager);
begin T := Self.ParentManager; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSoftManager_R(Self: cEmuteca; var T: cEmutecaSoftManager);
begin T := Self.SoftManager; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaConfig_W(Self: cEmuteca; const T: cEmutecaConfig);
begin Self.Config := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaConfig_R(Self: cEmuteca; var T: cEmutecaConfig);
begin T := Self.Config; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaTempFolder_W(Self: cEmuteca; const T: string);
begin Self.TempFolder := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaTempFolder_R(Self: cEmuteca; var T: string);
begin T := Self.TempFolder; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaProgressCallBack_W(Self: cEmuteca; const T: TEmutecaProgressCallBack);
begin Self.ProgressCallBack := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaProgressCallBack_R(Self: cEmuteca; var T: TEmutecaProgressCallBack);
begin T := Self.ProgressCallBack; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_cEmuteca(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmuteca) do
  begin
    RegisterPropertyHelper(@cEmutecaProgressCallBack_R,@cEmutecaProgressCallBack_W,'ProgressCallBack');
    RegisterPropertyHelper(@cEmutecaTempFolder_R,@cEmutecaTempFolder_W,'TempFolder');
    RegisterMethod(@cEmuteca.LoadConfig, 'LoadConfig');
    RegisterMethod(@cEmuteca.SearchParent, 'SearchParent');
    RegisterMethod(@cEmuteca.SearchSystem, 'SearchSystem');
    RegisterMethod(@cEmuteca.SearchMainEmulator, 'SearchMainEmulator');
    RegisterMethod(@cEmuteca.RunSoftware, 'RunSoftware');
    RegisterPropertyHelper(@cEmutecaConfig_R,@cEmutecaConfig_W,'Config');
    RegisterPropertyHelper(@cEmutecaSoftManager_R,nil,'SoftManager');
    RegisterPropertyHelper(@cEmutecaParentManager_R,nil,'ParentManager');
    RegisterPropertyHelper(@cEmutecaEmulatorManager_R,nil,'EmulatorManager');
    RegisterPropertyHelper(@cEmutecaSystemManager_R,nil,'SystemManager');
    RegisterPropertyHelper(@cEmutecaCurrentSoft_R,@cEmutecaCurrentSoft_W,'CurrentSoft');
    RegisterPropertyHelper(@cEmutecaCurrentParent_R,@cEmutecaCurrentParent_W,'CurrentParent');
    RegisterPropertyHelper(@cEmutecaCurrentEmulator_R,@cEmutecaCurrentEmulator_W,'CurrentEmulator');
    RegisterPropertyHelper(@cEmutecaCurrentSystem_R,@cEmutecaCurrentSystem_W,'CurrentSystem');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_ucEmuteca(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmuteca(CL);
end;

 
 
{ TPSImport_ucEmuteca }
(*----------------------------------------------------------------------------*)
procedure TPSImport_ucEmuteca.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_ucEmuteca(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_ucEmuteca.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ucEmuteca(ri);
end;
(*----------------------------------------------------------------------------*)
 
 
end.