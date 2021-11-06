unit CommandParser;

interface

uses Classes, Sysutils;

type
  TBracketSet = record
                 Open, Close: Char;
                end;
  TBracket = (brArgument, brCommand, brComment);

const
  NumBrackets = 3;

  BracketList: array[TBracket] of TBracketSet =
    ((Open: '('; Close: ')'),
     (Open: '['; Close: ']'),
     (Open: '{'; Close: '}')
    );

function ParseText(const Text: string): TStringList;

implementation


function ParseText;
var
 InBracket: Array[TBracket] of Integer;
 b: TBracket;
begin
  Result := TStringList.Create;
  try
   for b := brArgument to brComment do
     InBracket[b] := 0;

  except
    Result.Free;
    Result := nil;
    raise;
  end;
end;

end.
