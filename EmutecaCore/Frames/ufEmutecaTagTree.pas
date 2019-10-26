unit ufEmutecaTagTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, IniFiles,
  // CHX frames
  ufCHXTagTree,
  // Emuteca units
  ucEmutecaGroup;

type

  { TfmEmutecaTagTree }

  TfmEmutecaTagTree = class(TfmCHXTagTree)
  private
    FTagsIni: TMemIniFile;
    FCurrentGroup: cEmutecaGroup;
    procedure SetCurrentGroup(const AValue: cEmutecaGroup);

  protected

  public
    property CurrentGroup: cEmutecaGroup read FCurrentGroup
      write SetCurrentGroup;

    property TagsIni: TMemIniFile read FTagsIni;

    procedure UpdateTagsIni;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaTagTree }

procedure TfmEmutecaTagTree.SetCurrentGroup(const AValue: cEmutecaGroup);
begin
  if FCurrentGroup = AValue then
    Exit;
  FCurrentGroup := AValue;
end;

procedure TfmEmutecaTagTree.UpdateTagsIni;
var
  i, j: integer;
  TempIni: TMemIniFile;
  TempStrList, SecStrLst, Sections: TStringList;
begin
  if CheckedList.Count = 0 then
  begin
    FreeAndNil(FTagsIni);
    Exit;
  end;

  if not Assigned(FTagsIni) then
    FTagsIni := TMemIniFile.Create('', []);
  TagsIni.Clear;

  // First ini is straight copied
  TempIni := TMemIniFile.Create(CheckedList[0], False);
  TempStrList := TStringList.Create;
  TempIni.GetStrings(TempStrList);
  TagsIni.SetStrings(TempStrList);
  TempIni.Free;
  TempStrList.Free;

  Sections := TStringList.Create;
  Sections.CaseSensitive := False;
  // Sections are actually System IDs
  TagsIni.ReadSections(Sections);

  // Next inis must be compared
  i := 1;
  while i < CheckedList.Count do
  begin
    TempIni := TMemIniFile.Create(CheckedList[i], False);

    j := 0;
    while j < Sections.Count do
    begin
      if TempIni.SectionExists(Sections[j]) then
      begin
        //SecStrLst := TStringList.Create;
        //TagsIni.ReadSectionRaw(Sections[j], SecStrLst);
        //SecStrLst.CaseSensitive := False;
        //SecStrLst.Sorted := True;
        //
        //TempStrList:= TStringList.Create;
        //TempIni.ReadSectionRaw(Sections[j], TempStrList);
        //TempStrList.CaseSensitive := False;
        //TempStrList.Sorted := True;
        //
        //comparar y borrar lo que no coinciden;
        //
        //TagsIni.writesectionraw
        //
        //TempStrList.Free;
        //SecStrLst.Free;

        Inc(j);
      end
      else
      begin
        // Removing section
        TagsIni.EraseSection(Sections[j]);
        Sections.Delete(j);
      end;
    end;

    TempIni.Free;
    Inc(i);
  end;

Sections.Free;
end;

constructor TfmEmutecaTagTree.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FTagsIni := nil;
end;

destructor TfmEmutecaTagTree.Destroy;
begin
  FTagsIni.Free;

  inherited Destroy;
end;

end.
