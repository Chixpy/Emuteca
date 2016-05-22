unit ucEmutecaGameTag;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { cEmutecaTag }

  cEmutecaTag = class(TComponent)
  private
    FDescription: string;
    FTitle: string;
    procedure SetDescription(AValue: string);
    procedure SetTitle(AValue: string);

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Title: string read FTitle write SetTitle;
    {< Name of the tag or tag group. }
    property Description: string read FDescription write SetDescription;
    {< Description. }
  end;

implementation

{ cEmutecaTag }

procedure cEmutecaTag.SetDescription(AValue: string);
begin
  if FDescription = AValue then
    Exit;
  FDescription := AValue;
end;

procedure cEmutecaTag.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
end;

constructor cEmutecaTag.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor cEmutecaTag.Destroy;
begin
  inherited Destroy;
end;

end.
