program LazEmuteca;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, bs_controls, lz_rtticontrols, pl_pascalscript, pl_virtualtrees,
  ucEmuteca, uPSI_ucEmuteca, ucEmutecaParent, ucEmutecaConfig, ufEmutecaMain,
  ucEmutecaEmulatorManager, ucEmutecaParentManager,
  ucEmutecaSystemManager, ucEmutecaEmulator, ufEmutecaSoftList,
  ucEmutecaSoftware, ucEmutecaSystem, ufEmutecaEmulatorEditor,
  ufEmutecaEmulatorManager, ugEmutecaManager,
  ugEmutecaPersList, ufEmutecaScriptManager, ucEmutecaSoftManager,
ufEmutecaActAddSoft, ufEmutecaSoftEditor, uGUIConfig,
  uaEmutecaStorable, uaEmutecaManager, ufEmutecaSystemImgEditor,
  ufEmutecaActAddFolder, ucEmutecaScriptEngine,
  ufESMSoftList, ufEmutecaSystemCBX, ufESMParentList, ufEmutecaChkSoftList,
  ufEmutecaParentCBX, ufEmutecaIcnSoftList, ufCHXPropEditor, 
ufEmutecaSystemEditor, ufLEmuTKFullSystemEditor, ufCHXChkLstPropEditor, 
ufLEmuTKSysManager;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmEmutecaMain, frmEmutecaMain);
  Application.Run;
end.

