unit uPSI_u7zWrapper;
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
  TPSImport_u7zWrapper = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;
 
 
{ compile-time registration functions }
procedure SIRegister_u7zWrapper(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_u7zWrapper_Routines(S: TPSExec);

procedure Register;

implementation


uses
   FileUtil
  ,Process
  ,strutils
  ,u7zWrapper
  ;
 
 
procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_u7zWrapper]);
end;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_u7zWrapper(CL: TPSPascalCompiler);
begin
 CL.AddDelphiFunction('Function w7zListFiles( const aFilename : String; PackedFiles : TStrings; const OnlyPaths : boolean; const UseCache: boolean; const Password : String) : Integer');
 CL.AddDelphiFunction('Function w7zExtractFile( const a7zArchive : String; const aFileMask : String; aFolder : String; const ShowProgress : Boolean; const Password : String) : Integer');
 CL.AddDelphiFunction('Function w7zCompressFile( const a7zArchive : String; aFileList : TStrings; const ShowProgress : Boolean; const CompType : String) : Integer');
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure RIRegister_u7zWrapper_Routines(S: TPSExec);
begin
 S.RegisterDelphiFunction(@w7zListFiles, 'w7zListFiles', cdRegister);
 S.RegisterDelphiFunction(@w7zExtractFile, 'w7zExtractFile', cdRegister);
 S.RegisterDelphiFunction(@w7zCompressFile, 'w7zCompressFile', cdRegister);
end;

 
 
{ TPSImport_u7zWrapper }
(*----------------------------------------------------------------------------*)
procedure TPSImport_u7zWrapper.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_u7zWrapper(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_u7zWrapper.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
//  RIRegister_u7zWrapper(ri);
  RIRegister_u7zWrapper_Routines(CompExec.Exec); // comment it if no routines
end;
(*----------------------------------------------------------------------------*)
 
 
end.
