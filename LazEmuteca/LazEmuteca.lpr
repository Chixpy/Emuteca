program LazEmuteca;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, bs_controls, lz_rtticontrols, pl_pascalscript, pl_virtualtrees,
  ucEmuteca, ucEmutecaGroup, ucEmutecaConfig, ufrLEmuTKMain,
  ucEmutecaEmulatorManager, ucEmutecaGroupManager, ucEmutecaSystemManager,
  ucEmutecaEmulator, ufEmutecaSoftList, ucEmutecaSoftware, ucEmutecaSystem,
  ufEmutecaScriptManager,
  ucEmutecaSoftManager, uGUIConfig, uaCHXStorable, uaCHXConfig,
  uaEmutecaManager, ufEmutecaSystemImgEditor,
  ucEmutecaScriptEngine, ufESMSoftList, ufEmutecaSystemCBX, ufESMGroupList,
  ufLEmuTKChkSoftList, ufEmutecaGroupCBX, ufLEmuTKIcnSoftList, ufCHXPropEditor,
  ufEmutecaSystemEditor, ufLEmuTKFullSystemEditor, ufCHXChkLstPropEditor,
  ufLEmuTKSysManager, ufLEmuTKEmuManager, ufEmutecaSoftEditor,
  ufLEmuTKFullEmuEditor, ufEmutecaEmulatorEditor, ufLEmuTKSoftMedia,
  ufEmutecaActAddSoft, ufLEmuTKMain, ufLEmuTKPreviewList, ufCHXImgViewer,
  ufLEmuTKSoftImgPreview, ufLEmuTKSoftTxtPreview, ufCHXForm, ufLEmuTKIcnGrpList,
  ufrLEmuTKExportData, ufLEmuTKExportData, 
ufEmutecaActAddFolder, uCHXDlgUtils;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmLEmuTKMain, frmLEmuTKMain);
  Application.Run;
end.

