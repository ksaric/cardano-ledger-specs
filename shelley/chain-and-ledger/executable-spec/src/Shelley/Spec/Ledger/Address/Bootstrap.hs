class DSIGNAlgorithm c => BootstrapSigning v c where

  type KeyHashStuff v :: TYPE

  -- For a Byron era address a, it should be that hashKey stuff == addrRoot a
  hashKey 
    :: KeyHashStuff v -> KeyHash

  extractVerificationKey :: KeyHashStuff v -> VerKeyDSIGN c



verification key: arrives with witness
junk: arrives with address


ByronAddress = addrRoot + Some Junk

Junk ~ (NetworkMagic, Maybe ByteString, AddrType)
makeAddrRoot :: VerificationKey -> ChainCode -> Some Junk -> addrRoot

Witness ~ Signature + Key + Junk1
Addr ~ AddrRoot + Junk2

match :: Addr -> Witness -> Boolean

Set 1
addr1
addr2
...
addr n


Set 2
witness1
witness2
...
witness n



addrRoot ~ Hash Address'
newtype Address' = Address'
  { unAddress' :: (AddrType ✔︎, AddrSpendingData ✔︎, Attributes AddrAttributes (✔︎, ?, ?)
  } deriving (Eq, Show, Generic)

data AddrType  -- << sent with address
  = ATVerKey
  | ATRedeem
 
data Attributes h = Attributes  -- << sent with address
  { attrData   :: h
  , attrRemain :: UnparsedFields
  } deriving (Eq, Ord, Generic, NoUnexpectedThunks)

data AddrAttributes = AddrAttributes
    { aaVKDerivationPath  :: !(Maybe HDAddressPayload) -- <<< always Nothing?
    , aaNetworkMagic      :: !NetworkMagic -- <<<< network magic
    } deriving (Eq, Ord, Show, Generic, NFData, NoUnexpectedThunks)


data AddrSpendingData   -- << not sent with address
  = VerKeyASD !VerificationKey
  | RedeemASD !RedeemVerificationKey --- <<<<< we're prtending these dont exist
newtype VerificationKey = VerificationKey { unVerificationKey :: CC.XPub }
data XPub = XPub
    { xpubPublicKey :: !ByteString -- <<<<<< verification key
    , xpubChaincode :: !ChainCode
    }


addr -> addrroot
witness -> addrroot




instance BootstrapSigning BYRON where
  type SigningKey BYRON = Byron.SigningKey
  type VerificationKey BYRON = (Byron.VerificationKey, Junk ( incld chaincode))

deriveByronAddressRoot
  :: VerKeyDSIGN c
  -> ChainCode
  -> AddrRoot

checkByronAddressOwnership
  :: VerKeyDSIGN c
  -> ChainCode -- This comes separately off-chain
  -> Byron.Address -- This includes some stuff needed to make the 'addrRoot' from the verkey
  -> Bool
