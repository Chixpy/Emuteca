unit uPSI_ucEmutecaSystemManager;
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
  TPSImport_ucEmutecaSystemManager = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;
 
 
{ compile-time registration functions }
procedure SIRegister_cEmutecaSystemManager(CL: TPSPascalCompiler);
procedure SIRegister_ucEmutecaSystemManager(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_cEmutecaSystemManager(CL: TPSRuntimeClassImporter);
procedure RIRegister_ucEmutecaSystemManager(CL: TPSRuntimeClassImporter);

procedure Register;

implementation


uses
   fgl
  ,LazFileUtils
  ,LazUTF8
  ,IniFiles
  ,uCHXStrUtils
  ,uaCHXStorable
  ,uEmutecaCommon
  ,ucEmutecaSystemList
  ,ucEmutecaSystemManager
  ;
 
 
procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_ucEmutecaSystemManager]);
end;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_cEmutecaSystemManager(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caCHXStorableIni', 'cEmutecaSystemManager') do
  with CL.AddClassN(CL.FindClass('caCHXStorableIni'),'cEmutecaSystemManager') do
  begin
    RegisterProperty('TempFolder', 'string', iptrw);
    RegisterProperty('SysDataFolder', 'string', iptrw);
    RegisterMethod('Procedure ClearData');
    RegisterMethod('Procedure LoadData');
    RegisterMethod('Procedure SaveData');
    RegisterMethod('Procedure UpdateEnabledList');
    RegisterProperty('ProgressCallBack', 'TEmutecaProgressCallBack', iptrw);
    RegisterMethod('Procedure AssingAllTo( aList : TStrings)');
    RegisterMethod('Procedure AssingEnabledTo( aList : TStrings)');
    RegisterProperty('FullList', 'cEmutecaSystemList', iptr);
    RegisterProperty('EnabledList', 'cEmutecaSystemList', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_ucEmutecaSystemManager(CL: TPSPascalCompiler);
begin
  SIRegister_cEmutecaSystemManager(CL);
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemManagerEnabledList_R(Self: cEmutecaSystemManager; var T: cEmutecaSystemList);
begin T := Self.EnabledList; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemManagerFullList_R(Self: cEmutecaSystemManager; var T: cEmutecaSystemList);
begin T := Self.FullList; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemManagerProgressCallBack_W(Self: cEmutecaSystemManager; const T: TEmutecaProgressCallBack);
begin Self.ProgressCallBack := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemManagerProgressCallBack_R(Self: cEmutecaSystemManager; var T: TEmutecaProgressCallBack);
begin T := Self.ProgressCallBack; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemManagerSysDataFolder_W(Self: cEmutecaSystemManager; const T: string);
begin Self.SysDataFolder := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemManagerSysDataFolder_R(Self: cEmutecaSystemManager; var T: string);
begin T := Self.SysDataFolder; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemManagerTempFolder_W(Self: cEmutecaSystemManager; const T: string);
begin Self.TempFolder := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemManagerTempFolder_R(Self: cEmutecaSystemManager; var T: string);
begin T := Self.TempFolder; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_cEmutecaSystemManager(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaSystemManager) do
  begin
    RegisterPropertyHelper(@cEmutecaSystemManagerTempFolder_R,@cEmutecaSystemManagerTempFolder_W,'TempFolder');
    RegisterPropertyHelper(@cEmutecaSystemManagerSysDataFolder_R,@cEmutecaSystemManagerSysDataFolder_W,'SysDataFolder');
    RegisterMethod(@cEmutecaSystemManager.ClearData, 'ClearData');
    RegisterMethod(@cEmutecaSystemManager.LoadData, 'LoadData');
    RegisterMethod(@cEmutecaSystemManager.SaveData, 'SaveData');
    RegisterMethod(@cEmutecaSystemManager.UpdateEnabledList, 'UpdateEnabledList');
    RegisterPropertyHelper(@cEmutecaSystemManagerProgressCallBack_R,@cEmutecaSystemManagerProgressCallBack_W,'ProgressCallBack');
    RegisterPropertyHelper(@cEmutecaSystemManagerFullList_R,nil,'FullList');
    RegisterPropertyHelper(@cEmutecaSystemManagerEnabledList_R,nil,'EnabledList');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_ucEmutecaSystemManager(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaSystemManager(CL);
end;

 
 
{ TPSImport_ucEmutecaSystemManager }
(*----------------------------------------------------------------------------*)
procedure TPSImport_ucEmutecaSystemManager.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_ucEmutecaSystemManager(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_ucEmutecaSystemManager.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ucEmutecaSystemManager(ri);
end;
(*----------------------------------------------------------------------------*)
 
 
end.