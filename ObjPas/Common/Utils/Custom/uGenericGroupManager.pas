unit uGenericGroupManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  { cGenObjGroupManager.

  This class manages a generic pseudo-tree of owned groups and items.

  Not teorical implementation, only crazy shit testing generics.

  Groups are handled internally and GroupIDs can't repeat.

  ParentGroup1/ChildGroup1
  ParentGroup2/ChildGroup1
  ParentGroup3/ParentGroup31/ChildGroup1

  ChildGroup1 will be the same group in all cases.
  }

  generic cGenObjGroupManager<TGOGMGroupKey, TGOGMItemKey, TGOGMItem> = class(TObject)
  private
    type
      // Items handling
      TItemOwnedList = specialize TFPGObjectList<TGOGMItem>;
      TItemList = specialize TFPGObjectList<TGOGMItem>;
      TItemMap = specialize TFPGMap<TGOGMItemKey, TGOGMItem>;

      // Groups handling
      TGroup = class;
      TGroupOwnedList = specialize TFPGObjectList<TGroup>;
      TGroupList = specialize TFPGList<TGroup>;
      TGroupMap = specialize TFPGMap<TGOGMGroupKey, TGroup>;

      { TGroup }

      TGroup = class (TObject)
      private
        FItems: TItemMap;
        FSubgroups: TGroupMap;

      public
        property Items: TItemMap read FItems;
        property Subgroups: TGroupMap read FSubgroups;
      end;

  private
    FGroups: TGroupMap;
    FItems: TItemMap;
    Tree: TGroup;
    FOwnedGroupList: TGroupOwnedList;
    FOwnedItemList: TItemOwnedList;

  protected
    property OwnedItemList: TItemOwnedList read FOwnedItemList;
    property OwnedGroupList: TGroupOwnedList read FOwnedGroupList;

  public
    property Tree: TGroup read Tree;
    property Items: TItemMap read FItems;
    property Groups: TGroupMap read FGroups;

    procedure AddRootGroup(GroupKey: TGOGMGroupKey);
    procedure AddSubGroup(ParentKey, ChildKey: TGOGMGroupKey);
    procedure AddItem(GroupKey: TGOGMGroupKey; ItemKey: TGOGMItemKey;
      aItem: TGOGMItem);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

function cGenObjGroupManager.AddGroup(TGOGMGroupKey): TGroup;
var
  aIndex: Integer;
begin
  if not Groups.Find(aGroupKey, aIndex) then
  begin
    Result := TGroup.Create;
    Groups.Add(aGroupKey, Result);
  end
  else
  begin
    Result := Groups.Data[aIndex];
  end;
end;

procedure cGenObjGroupManager.AddRootGroup(GroupKey: TGOGMGroupKey);
var
  aGroup: TGroup;
begin
  aGroup := AddGroup(GroupKey);
  Groups.Add(GroupKey, aGroup);
end;

procedure cGenObjGroupManager.AddSubGroup(ParentKey, ChildKey: TGOGMGroupKey);
var
  aParent, aChild: TGroup;
begin
  aParent := AddGroup(ParentKey);
  aChild := AddGroup(ChildKey);
  aParent.Subgroups.Add(ChildKey, aChild);
end;

procedure cGenObjGroupManager.AddItem(GroupKey: TGOGMGroupKey;
  ItemKey: TGOGMItemKey; aItem: TGOGMItem);
var
  aGroup: TGroup;
begin
  aGroup := AddGroup(GroupKey);
  aGroup.Items.Add(ItemKey, aItem);
end;

constructor cGenObjGroupManager.Create;
begin
  inherited Create;
  FOwnedItemList:= TItemOwnedList.Create;
  FOwnedGroupList := TGroupOwnedList.Create;
  FGroups := TGroupMap.create;
  FItems := TItemMap.Create;
  Tree := TGroup.Create;
end;

destructor cGenObjGroupManager.Destroy;
begin
  FreeAndNil(FGroups);
  FreeAndNil(FItems);
  FreeAndNil(Tree);
  FreeAndNil(FOwnedGroupList);
  FreeAndNil(FOwnedItemList);
  inherited Destroy;
end;

end.

