Unprotect[SymmetrizedArray, QuantityArray];

SymmetrizedArray[StructuredArray`StructuredData[dims_, {args__}]] := 
    StructuredArray[SymmetrizedArray, dims, StructuredArray`StructuredData[SymmetrizedArray, args]];

QuantityArray[StructuredArray`StructuredData[dims_, {args__}]] := 
    StructuredArray[QuantityArray, dims, StructuredArray`StructuredData[QuantityArray, args]];

Protect[SymmetrizedArray, QuantityArray];
