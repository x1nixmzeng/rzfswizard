{
  rzfswizard
  x1nixmzeng (July 2012)

  Ported to Pascal from libMSF.c

  July 21 2012
    Added msfChecksum() function
}
unit libMSF;

interface
  function msfGetHashTable( ): Boolean;
  procedure msfClearHashTable( );
  procedure msfMakeHashTable( );
  function msfScramble(var data: array of byte; size: cardinal ): integer;
  function msfUnscramble(var dataBuf: array of byte; size: cardinal ): integer;
  function msfChecksum(var dataBuf: array of byte; size: cardinal): word;

implementation

const MSF_XORTABLE_SIZE : Cardinal = $10000;

var g_msf_xortable : array of byte = nil;

function msfGetHashTable( ): Boolean;
begin
  Result := g_msf_xortable <> nil;
end;

procedure msfClearHashTable( );
begin
  SetLength( g_msf_xortable, 0 );
end;

procedure msfMakeHashTable( );
var
  i,j,
  r,poly : Cardinal; // unsigned
const
  POLYNOMINAL : Cardinal = $85F24C5A;
begin

  if not msfGetHashTable() then
  begin
    SetLength(g_msf_xortable, MSF_XORTABLE_SIZE);

    for i:=0 to (MSF_XORTABLE_SIZE-1) do
    begin
      r := i+1;

      for j:=0 to 7 do
      begin
        poly := (not ((r and 1) -1)) and POLYNOMINAL;
        r := (r shr 1) xor poly;
      end;

      g_msf_xortable[i] := r;
    end;

  end;
end;

function msfScramble(var data: array of byte; size: cardinal ): integer;
var
  i: Cardinal;
begin
  Result := 0;

  if ( ( msfGetHashTable() ) and ( size > 0 ) ) then
  begin

    data[0] := data[0] xor ( (data[size-1] shr 1) xor g_msf_xortable[0] );

    for i := 1 to size-1 do
      data[i] := data[i] xor ( (data[i-1] shr 1) xor g_msf_xortable[i and $FFFF] );

    Result := size;
    
  end;
end;

function msfUnscramble(var dataBuf: array of byte; size: cardinal ): integer;
var
  i: Cardinal;
begin
  Result := 0;

  if ( ( msfGetHashTable() ) and ( size > 0 ) ) then
  begin

    for i := size-1 downto 1 do
      dataBuf[i] := dataBuf[i] xor ( (dataBuf[i-1] shr 1) xor g_msf_xortable[i and $FFFF] );

    dataBuf[0] := dataBuf[0] xor ( (dataBuf[size-1] shr 1) xor g_msf_xortable[0] );

    Result := size;
    
  end;
end;

function msfChecksum(var dataBuf: array of byte; size: cardinal): word;
var
  i: Cardinal;
begin
  // Base checksum value
  Result := $3F75;

  if ( ( msfGetHashTable() ) and ( size > 0 ) ) then
  begin

    for i := 0 to size-1 do
      Result := ( Result shl 1 ) + g_msf_xortable[ dataBuf[i] ];

  end;

end;

end.

