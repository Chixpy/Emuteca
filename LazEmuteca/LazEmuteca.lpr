program LazEmuteca;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, bs_controls, lz_rtticontrols, pl_pascalscript, pl_virtualtrees,
  ucEmuteca, uPSI_ucEmuteca, ucEmutecaGroup, ucEmutecaConfig, ufEmutecaMain,
  ucEmutecaEmulatorManager, ucEmutecaGroupManager,
  ucEmutecaSystemManager, ucEmutecaEmulator, ufEmutecaSoftList,
  ucEmutecaSoftware, ucEmutecaSystem,
  ugEmutecaManager,
  ugEmutecaPersList, ufEmutecaScriptManager, ucEmutecaSoftManager,
uGUIConfig,
  uaCHXStorable, uaEmutecaManager, ufEmutecaSystemImgEditor,
  ufEmutecaActAddFolder, ucEmutecaScriptEngine,
  ufESMSoftList, ufEmutecaSystemCBX, ufESMGroupList, ufLEmuTKChkSoftList,
  ufEmutecaGroupCBX, ufLEmuTKIcnSoftList, ufCHXPropEditor, 
ufEmutecaSystemEditor, ufLEmuTKFullSystemEditor, ufCHXChkLstPropEditor, 
ufLEmuTKSysManager, ufLEmuTKEmuManager, ufEmutecaSoftEditor, 
ufLEmuTKFullEmuEditor, ufEmutecaEmulatorEditor, ufLEmuTKSoftMedia, 
ufEmutecaActAddSoft;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmEmutecaMain, frmEmutecaMain);
  Application.Run;
end.

