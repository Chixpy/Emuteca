unit uPSI_uaEmutecaCustomSystem;
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
  TPSImport_uaEmutecaCustomSystem = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;
 
 
{ compile-time registration functions }
procedure SIRegister_caEmutecaCustomSystem(CL: TPSPascalCompiler);
procedure SIRegister_uaEmutecaCustomSystem(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_caEmutecaCustomSystem(CL: TPSRuntimeClassImporter);
procedure RIRegister_uaEmutecaCustomSystem(CL: TPSRuntimeClassImporter);

procedure Register;

implementation


uses
   IniFiles
  ,LazFileUtils
  ,LazUTF8
  ,uCHXStrUtils
  ,uaCHXStorable
  ,ucCHXImageList
  ,uEmutecaCommon
  ,ucEmutecaPlayingStats
  ,uaEmutecaCustomSystem
  ;
 
 
procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_uaEmutecaCustomSystem]);
end;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_caEmutecaCustomSystem(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caCHXStorableIni', 'caEmutecaCustomSystem') do
  with CL.AddClassN(CL.FindClass('caCHXStorableIni'),'caEmutecaCustomSystem') do
  begin
    RegisterProperty('TempFolder', 'string', iptrw);
    RegisterMethod('Procedure CacheIcon( aImagList : cCHXImageList)');
    RegisterProperty('ID', 'string', iptrw);
    RegisterProperty('Title', 'string', iptrw);
    RegisterProperty('FileName', 'string', iptrw);
    RegisterProperty('Enabled', 'boolean', iptrw);
    RegisterProperty('ExtractAll', 'boolean', iptrw);
    RegisterProperty('BaseFolder', 'string', iptrw);
    RegisterProperty('WorkingFolder', 'string', iptrw);
    RegisterProperty('MainEmulator', 'string', iptrw);
    RegisterProperty('OtherEmulators', 'TStringList', iptr);
    RegisterProperty('Icon', 'string', iptrw);
    RegisterProperty('Image', 'string', iptrw);
    RegisterProperty('BackImage', 'string', iptrw);
    RegisterProperty('IconFolder', 'string', iptrw);
    RegisterProperty('ImageFolders', 'TStringList', iptr);
    RegisterProperty('ImageCaptions', 'TStringList', iptr);
    RegisterProperty('InfoText', 'string', iptrw);
    RegisterProperty('TextFolders', 'TStringList', iptr);
    RegisterProperty('TextCaptions', 'TStringList', iptr);
    RegisterProperty('MusicFolders', 'TStringList', iptr);
    RegisterProperty('MusicCaptions', 'TStringList', iptr);
    RegisterProperty('VideoFolders', 'TStringList', iptr);
    RegisterProperty('VideoCaptions', 'TStringList', iptr);
    RegisterProperty('SoftExportKey', 'TEmutecaSoftExportKey', iptrw);
    RegisterProperty('Extensions', 'TStringList', iptr);
    RegisterProperty('Stats', 'cEmutecaPlayingStats', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_uaEmutecaCustomSystem(CL: TPSPascalCompiler);
begin
  SIRegister_caEmutecaCustomSystem(CL);
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemStats_R(Self: caEmutecaCustomSystem; var T: cEmutecaPlayingStats);
begin T := Self.Stats; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemExtensions_R(Self: caEmutecaCustomSystem; var T: TStringList);
begin T := Self.Extensions; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemSoftExportKey_W(Self: caEmutecaCustomSystem; const T: TEmutecaSoftExportKey);
begin Self.SoftExportKey := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemSoftExportKey_R(Self: caEmutecaCustomSystem; var T: TEmutecaSoftExportKey);
begin T := Self.SoftExportKey; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemVideoCaptions_R(Self: caEmutecaCustomSystem; var T: TStringList);
begin T := Self.VideoCaptions; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemVideoFolders_R(Self: caEmutecaCustomSystem; var T: TStringList);
begin T := Self.VideoFolders; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemMusicCaptions_R(Self: caEmutecaCustomSystem; var T: TStringList);
begin T := Self.MusicCaptions; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemMusicFolders_R(Self: caEmutecaCustomSystem; var T: TStringList);
begin T := Self.MusicFolders; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemTextCaptions_R(Self: caEmutecaCustomSystem; var T: TStringList);
begin T := Self.TextCaptions; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemTextFolders_R(Self: caEmutecaCustomSystem; var T: TStringList);
begin T := Self.TextFolders; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemInfoText_W(Self: caEmutecaCustomSystem; const T: string);
begin Self.InfoText := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemInfoText_R(Self: caEmutecaCustomSystem; var T: string);
begin T := Self.InfoText; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemImageCaptions_R(Self: caEmutecaCustomSystem; var T: TStringList);
begin T := Self.ImageCaptions; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemImageFolders_R(Self: caEmutecaCustomSystem; var T: TStringList);
begin T := Self.ImageFolders; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemIconFolder_W(Self: caEmutecaCustomSystem; const T: string);
begin Self.IconFolder := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemIconFolder_R(Self: caEmutecaCustomSystem; var T: string);
begin T := Self.IconFolder; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemBackImage_W(Self: caEmutecaCustomSystem; const T: string);
begin Self.BackgroundFile := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemBackImage_R(Self: caEmutecaCustomSystem; var T: string);
begin T := Self.BackgroundFile; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemImage_W(Self: caEmutecaCustomSystem; const T: string);
begin Self.ImageFile := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemImage_R(Self: caEmutecaCustomSystem; var T: string);
begin T := Self.ImageFile; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemIcon_W(Self: caEmutecaCustomSystem; const T: string);
begin Self.IconFile := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemIcon_R(Self: caEmutecaCustomSystem; var T: string);
begin T := Self.IconFile; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemOtherEmulators_R(Self: caEmutecaCustomSystem; var T: TStringList);
begin T := Self.OtherEmulators; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemMainEmulator_W(Self: caEmutecaCustomSystem; const T: string);
begin Self.MainEmulator := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemMainEmulator_R(Self: caEmutecaCustomSystem; var T: string);
begin T := Self.MainEmulator; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemWorkingFolder_W(Self: caEmutecaCustomSystem; const T: string);
begin Self.WorkingFolder := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemWorkingFolder_R(Self: caEmutecaCustomSystem; var T: string);
begin T := Self.WorkingFolder; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemBaseFolder_W(Self: caEmutecaCustomSystem; const T: string);
begin Self.BaseFolder := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemBaseFolder_R(Self: caEmutecaCustomSystem; var T: string);
begin T := Self.BaseFolder; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemExtractAll_W(Self: caEmutecaCustomSystem; const T: boolean);
begin Self.ExtractAll := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemExtractAll_R(Self: caEmutecaCustomSystem; var T: boolean);
begin T := Self.ExtractAll; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemEnabled_W(Self: caEmutecaCustomSystem; const T: boolean);
begin Self.Enabled := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemEnabled_R(Self: caEmutecaCustomSystem; var T: boolean);
begin T := Self.Enabled; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemFileName_W(Self: caEmutecaCustomSystem; const T: string);
begin Self.ListFileName := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemFileName_R(Self: caEmutecaCustomSystem; var T: string);
begin T := Self.ListFileName; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemTitle_W(Self: caEmutecaCustomSystem; const T: string);
begin Self.Title := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemTitle_R(Self: caEmutecaCustomSystem; var T: string);
begin T := Self.Title; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemID_W(Self: caEmutecaCustomSystem; const T: string);
begin Self.ID := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemID_R(Self: caEmutecaCustomSystem; var T: string);
begin T := Self.ID; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemTempFolder_W(Self: caEmutecaCustomSystem; const T: string);
begin Self.TempFolder := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSystemTempFolder_R(Self: caEmutecaCustomSystem; var T: string);
begin T := Self.TempFolder; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_caEmutecaCustomSystem(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(caEmutecaCustomSystem) do
  begin
    RegisterPropertyHelper(@caEmutecaCustomSystemTempFolder_R,@caEmutecaCustomSystemTempFolder_W,'TempFolder');
    RegisterMethod(@caEmutecaCustomSystem.CacheIcon, 'CacheIcon');
    RegisterPropertyHelper(@caEmutecaCustomSystemID_R,@caEmutecaCustomSystemID_W,'ID');
    RegisterPropertyHelper(@caEmutecaCustomSystemTitle_R,@caEmutecaCustomSystemTitle_W,'Title');
    RegisterPropertyHelper(@caEmutecaCustomSystemFileName_R,@caEmutecaCustomSystemFileName_W,'FileName');
    RegisterPropertyHelper(@caEmutecaCustomSystemEnabled_R,@caEmutecaCustomSystemEnabled_W,'Enabled');
    RegisterPropertyHelper(@caEmutecaCustomSystemExtractAll_R,@caEmutecaCustomSystemExtractAll_W,'ExtractAll');
    RegisterPropertyHelper(@caEmutecaCustomSystemBaseFolder_R,@caEmutecaCustomSystemBaseFolder_W,'BaseFolder');
    RegisterPropertyHelper(@caEmutecaCustomSystemWorkingFolder_R,@caEmutecaCustomSystemWorkingFolder_W,'WorkingFolder');
    RegisterPropertyHelper(@caEmutecaCustomSystemMainEmulator_R,@caEmutecaCustomSystemMainEmulator_W,'MainEmulator');
    RegisterPropertyHelper(@caEmutecaCustomSystemOtherEmulators_R,nil,'OtherEmulators');
    RegisterPropertyHelper(@caEmutecaCustomSystemIcon_R,@caEmutecaCustomSystemIcon_W,'Icon');
    RegisterPropertyHelper(@caEmutecaCustomSystemImage_R,@caEmutecaCustomSystemImage_W,'Image');
    RegisterPropertyHelper(@caEmutecaCustomSystemBackImage_R,@caEmutecaCustomSystemBackImage_W,'BackImage');
    RegisterPropertyHelper(@caEmutecaCustomSystemIconFolder_R,@caEmutecaCustomSystemIconFolder_W,'IconFolder');
    RegisterPropertyHelper(@caEmutecaCustomSystemImageFolders_R,nil,'ImageFolders');
    RegisterPropertyHelper(@caEmutecaCustomSystemImageCaptions_R,nil,'ImageCaptions');
    RegisterPropertyHelper(@caEmutecaCustomSystemInfoText_R,@caEmutecaCustomSystemInfoText_W,'InfoText');
    RegisterPropertyHelper(@caEmutecaCustomSystemTextFolders_R,nil,'TextFolders');
    RegisterPropertyHelper(@caEmutecaCustomSystemTextCaptions_R,nil,'TextCaptions');
    RegisterPropertyHelper(@caEmutecaCustomSystemMusicFolders_R,nil,'MusicFolders');
    RegisterPropertyHelper(@caEmutecaCustomSystemMusicCaptions_R,nil,'MusicCaptions');
    RegisterPropertyHelper(@caEmutecaCustomSystemVideoFolders_R,nil,'VideoFolders');
    RegisterPropertyHelper(@caEmutecaCustomSystemVideoCaptions_R,nil,'VideoCaptions');
    RegisterPropertyHelper(@caEmutecaCustomSystemSoftExportKey_R,@caEmutecaCustomSystemSoftExportKey_W,'SoftExportKey');
    RegisterPropertyHelper(@caEmutecaCustomSystemExtensions_R,nil,'Extensions');
    RegisterPropertyHelper(@caEmutecaCustomSystemStats_R,nil,'Stats');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_uaEmutecaCustomSystem(CL: TPSRuntimeClassImporter);
begin
  RIRegister_caEmutecaCustomSystem(CL);
end;

 
 
{ TPSImport_uaEmutecaCustomSystem }
(*----------------------------------------------------------------------------*)
procedure TPSImport_uaEmutecaCustomSystem.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_uaEmutecaCustomSystem(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_uaEmutecaCustomSystem.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  RIRegister_uaEmutecaCustomSystem(ri);
end;
(*----------------------------------------------------------------------------*)
 
 
end.
