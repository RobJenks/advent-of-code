if [ -z "$SUDO_USER" ]
then
	echo 'Error: Script must be run with elevated (sudo) permissions'
	exit 1
else
	echo "Executing installation script as \"${SUDO_USER}\""
fi

# Elevated permissions
echo "Updating pacman package repositories..."
pacman -Sy

echo "Installing GHC and dependencies..."
pacman -S ghc ghc-static ghc-libs libffi cabal-install

# Drop permissions to user-level
echo "Installing required cabal packages"
sudo -u ${SUDO_USER} cabal update
# sudo -u ${SUDO_USER} cabal install <dependencies>

echo "Installation complete"
exit 0
