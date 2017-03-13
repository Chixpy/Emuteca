unit uPSI_ucEmutecaSystem;
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
  TPSImport_ucEmutecaSystem = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;
 
 
{ compile-time registration functions }
procedure SIRegister_cEmutecaSystem(CL: TPSPascalCompiler);
procedure SIRegister_ucEmutecaSystem(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_ucEmutecaSystem_Routines(S: TPSExec);
procedure RIRegister_cEmutecaSystem(CL: TPSRuntimeClassImporter);
procedure RIRegister_ucEmutecaSystem(CL: TPSRuntimeClassImporter);

procedure Register;

implementation


uses
   IniFiles
  ,LazFileUtils
  ,LazUTF8
  ,contnrs
  ,uCHXStrUtils
  ,uEmutecaCommon
  ,uaCHXStorable
  ,ucEmutecaPlayingStats
  ,ucEmutecaSystem
  ;
 
 
procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_ucEmutecaSystem]);
end;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_cEmutecaSystem(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caCHXStorable', 'cEmutecaSystem') do
  with CL.AddClassN(CL.FindClass('caCHXStorable'),'cEmutecaSystem') do
  begin
    RegisterProperty('ID', 'string', iptrw);
    RegisterProperty('Title', 'string', iptrw);
    RegisterProperty('FileName', 'string', iptrw);
    RegisterProperty('Enabled', 'boolean', iptrw);
    RegisterProperty('ExtractAll', 'boolean', iptrw);
    RegisterProperty('BaseFolder', 'string', iptrw);
    RegisterProperty('TempFolder', 'string', iptrw);
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
    RegisterProperty('GameKey', 'TEmutecaFileKey', iptrw);
    RegisterProperty('Extensions', 'TStringList', iptr);
    RegisterProperty('Stats', 'cEmutecaPlayingStats', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_ucEmutecaSystem(CL: TPSPascalCompiler);
begin
 CL.AddConstantN('krsIniKeyEnabled','String').SetString( 'Enabled');
 CL.AddConstantN('krsIniKeyTitle','String').SetString( 'Title');
 CL.AddConstantN('krsIniKeyFileName','String').SetString( 'FileName');
 CL.AddConstantN('krsIniKeyExtensions','String').SetString( 'Extensions');
 CL.AddConstantN('krsIniKeyBaseFolder','String').SetString( 'BaseFolder');
 CL.AddConstantN('krsIniKeyTempFolder','String').SetString( 'TempFolder');
 CL.AddConstantN('krsIniKeyMainEmulator','String').SetString( 'MainEmulator');
 CL.AddConstantN('krsIniKeyOtherEmulators','String').SetString( 'OtherEmulators');
 CL.AddConstantN('krsIniKeyIcon','String').SetString( 'Icon');
 CL.AddConstantN('krsIniKeyImage','String').SetString( 'Image');
 CL.AddConstantN('krsIniKeyBackImage','String').SetString( 'BackImage');
 CL.AddConstantN('krsIniKeyIconFolder','String').SetString( 'IconFolder');
 CL.AddConstantN('krsIniKeyImageFolders','String').SetString( 'ImageFolders');
 CL.AddConstantN('krsIniKeyImageCaptions','String').SetString( 'ImageCaptions');
 CL.AddConstantN('krsIniKeyText','String').SetString( 'Text');
 CL.AddConstantN('krsIniKeyTextFolders','String').SetString( 'TextFolders');
 CL.AddConstantN('krsIniKeyTextCaptions','String').SetString( 'TextCaptions');
 CL.AddConstantN('krsIniKeyGamesKey','String').SetString( 'GamesKey');
 CL.AddConstantN('krsIniKeyExtractAll','String').SetString( 'ExtractAll');
 CL.AddConstantN('rsLoadingSystemList','String').SetString( 'Loading system list...');
 CL.AddConstantN('rsSavingSystemList','String').SetString( 'Saving system list...');
 CL.AddConstantN('rsAllSystems','String').SetString( 'All Systems');
 CL.AddConstantN('reSelectSystem','String').SetString( 'Select a System');
  CL.AddTypeS('TEmutecaFileKey', '( TEFKSHA1, TEFKCRC32, TEFKFileName, TEFKCust'
   +'om )');
  SIRegister_cEmutecaSystem(CL);
  CL.AddTypeS('cEmutecaSystemList', 'TComponentList');
  CL.AddTypeS('TEmutecaReturnSystemCB', 'Function ( aSystem : cEmutecaSystem) :'
   +' boolean');
 CL.AddDelphiFunction('Function EmutecaFileKey2Str( aEFK : TEmutecaFileKey) : string');
 CL.AddDelphiFunction('Function Str2EmutecaFileKey( aString : string) : TEmutecaFileKey');
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemStats_R(Self: cEmutecaSystem; var T: cEmutecaPlayingStats);
begin T := Self.Stats; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemExtensions_R(Self: cEmutecaSystem; var T: TStringList);
begin T := Self.Extensions; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemGameKey_W(Self: cEmutecaSystem; const T: TEmutecaFileKey);
begin Self.GameKey := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemGameKey_R(Self: cEmutecaSystem; var T: TEmutecaFileKey);
begin T := Self.GameKey; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemTextCaptions_R(Self: cEmutecaSystem; var T: TStringList);
begin T := Self.TextCaptions; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemTextFolders_R(Self: cEmutecaSystem; var T: TStringList);
begin T := Self.TextFolders; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemInfoText_W(Self: cEmutecaSystem; const T: string);
begin Self.InfoText := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemInfoText_R(Self: cEmutecaSystem; var T: string);
begin T := Self.InfoText; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemImageCaptions_R(Self: cEmutecaSystem; var T: TStringList);
begin T := Self.ImageCaptions; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemImageFolders_R(Self: cEmutecaSystem; var T: TStringList);
begin T := Self.ImageFolders; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemIconFolder_W(Self: cEmutecaSystem; const T: string);
begin Self.IconFolder := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemIconFolder_R(Self: cEmutecaSystem; var T: string);
begin T := Self.IconFolder; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemBackImage_W(Self: cEmutecaSystem; const T: string);
begin Self.BackImage := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemBackImage_R(Self: cEmutecaSystem; var T: string);
begin T := Self.BackImage; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemImage_W(Self: cEmutecaSystem; const T: string);
begin Self.Image := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemImage_R(Self: cEmutecaSystem; var T: string);
begin T := Self.Image; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemIcon_W(Self: cEmutecaSystem; const T: string);
begin Self.Icon := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemIcon_R(Self: cEmutecaSystem; var T: string);
begin T := Self.Icon; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemOtherEmulators_R(Self: cEmutecaSystem; var T: TStringList);
begin T := Self.OtherEmulators; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemMainEmulator_W(Self: cEmutecaSystem; const T: string);
begin Self.MainEmulator := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemMainEmulator_R(Self: cEmutecaSystem; var T: string);
begin T := Self.MainEmulator; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemTempFolder_W(Self: cEmutecaSystem; const T: string);
begin Self.TempFolder := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemTempFolder_R(Self: cEmutecaSystem; var T: string);
begin T := Self.TempFolder; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemBaseFolder_W(Self: cEmutecaSystem; const T: string);
begin Self.BaseFolder := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemBaseFolder_R(Self: cEmutecaSystem; var T: string);
begin T := Self.BaseFolder; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemExtractAll_W(Self: cEmutecaSystem; const T: boolean);
begin Self.ExtractAll := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemExtractAll_R(Self: cEmutecaSystem; var T: boolean);
begin T := Self.ExtractAll; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemEnabled_W(Self: cEmutecaSystem; const T: boolean);
begin Self.Enabled := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemEnabled_R(Self: cEmutecaSystem; var T: boolean);
begin T := Self.Enabled; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemFileName_W(Self: cEmutecaSystem; const T: string);
begin Self.FileName := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemFileName_R(Self: cEmutecaSystem; var T: string);
begin T := Self.FileName; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemTitle_W(Self: cEmutecaSystem; const T: string);
begin Self.Title := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemTitle_R(Self: cEmutecaSystem; var T: string);
begin T := Self.Title; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemID_W(Self: cEmutecaSystem; const T: string);
begin Self.ID := T; end;

(*----------------------------------------------------------------------------*)
procedure cEmutecaSystemID_R(Self: cEmutecaSystem; var T: string);
begin T := Self.ID; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_ucEmutecaSystem_Routines(S: TPSExec);
begin
 S.RegisterDelphiFunction(@Str2EmutecaFileKey, 'Str2EmutecaFileKey', cdRegister);
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_cEmutecaSystem(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaSystem) do
  begin
    RegisterPropertyHelper(@cEmutecaSystemID_R,@cEmutecaSystemID_W,'ID');
    RegisterPropertyHelper(@cEmutecaSystemTitle_R,@cEmutecaSystemTitle_W,'Title');
    RegisterPropertyHelper(@cEmutecaSystemFileName_R,@cEmutecaSystemFileName_W,'FileName');
    RegisterPropertyHelper(@cEmutecaSystemEnabled_R,@cEmutecaSystemEnabled_W,'Enabled');
    RegisterPropertyHelper(@cEmutecaSystemExtractAll_R,@cEmutecaSystemExtractAll_W,'ExtractAll');
    RegisterPropertyHelper(@cEmutecaSystemBaseFolder_R,@cEmutecaSystemBaseFolder_W,'BaseFolder');
    RegisterPropertyHelper(@cEmutecaSystemTempFolder_R,@cEmutecaSystemTempFolder_W,'TempFolder');
    RegisterPropertyHelper(@cEmutecaSystemMainEmulator_R,@cEmutecaSystemMainEmulator_W,'MainEmulator');
    RegisterPropertyHelper(@cEmutecaSystemOtherEmulators_R,nil,'OtherEmulators');
    RegisterPropertyHelper(@cEmutecaSystemIcon_R,@cEmutecaSystemIcon_W,'Icon');
    RegisterPropertyHelper(@cEmutecaSystemImage_R,@cEmutecaSystemImage_W,'Image');
    RegisterPropertyHelper(@cEmutecaSystemBackImage_R,@cEmutecaSystemBackImage_W,'BackImage');
    RegisterPropertyHelper(@cEmutecaSystemIconFolder_R,@cEmutecaSystemIconFolder_W,'IconFolder');
    RegisterPropertyHelper(@cEmutecaSystemImageFolders_R,nil,'ImageFolders');
    RegisterPropertyHelper(@cEmutecaSystemImageCaptions_R,nil,'ImageCaptions');
    RegisterPropertyHelper(@cEmutecaSystemInfoText_R,@cEmutecaSystemInfoText_W,'InfoText');
    RegisterPropertyHelper(@cEmutecaSystemTextFolders_R,nil,'TextFolders');
    RegisterPropertyHelper(@cEmutecaSystemTextCaptions_R,nil,'TextCaptions');
    RegisterPropertyHelper(@cEmutecaSystemGameKey_R,@cEmutecaSystemGameKey_W,'GameKey');
    RegisterPropertyHelper(@cEmutecaSystemExtensions_R,nil,'Extensions');
    RegisterPropertyHelper(@cEmutecaSystemStats_R,nil,'Stats');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_ucEmutecaSystem(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaSystem(CL);
end;

 
 
{ TPSImport_ucEmutecaSystem }
(*----------------------------------------------------------------------------*)
procedure TPSImport_ucEmutecaSystem.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_ucEmutecaSystem(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_ucEmutecaSystem.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ucEmutecaSystem(ri);
  RIRegister_ucEmutecaSystem_Routines(CompExec.Exec); // comment it if no routines
end;
(*----------------------------------------------------------------------------*)
 
 
end.
