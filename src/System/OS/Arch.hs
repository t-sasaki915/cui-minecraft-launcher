{-# LANGUAGE CPP #-}

module System.OS.Arch (OSArch (..), currentOSArch) where

data OSArch = X86 | X86_64 deriving (Show, Eq)

currentOSArch :: OSArch
#ifdef x86_64_HOST_ARCH
currentOSArch = X86_64
#endif
#ifdef i386_HOST_ARCH
currentOSArch = X86
#endif
