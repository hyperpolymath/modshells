def main [] {
    let hook_dir = ".git/hooks"
    let target_hook = ($hook_dir | path join "post-checkout")
    print "Ensuring .git/hooks directory exists..."
    ^mkdir -p $hook_dir
    print "Creating universal post-checkout launcher in .git/hooks..."
    let bash_hook_content = "#!/bin/bash\n# Universal launcher for post-checkout hook (safer-perms)\n\nif command -v nu >/dev/null 2>&1; then\n    HOOK_SCRIPT=\"scripts/safer-perms.nu\"\n    if [ -f \"$HOOK_SCRIPT\" ]; then\n        echo \"Executing Nushell-based post-checkout hook (safer-perms)...\"\n        nu --file \"$HOOK_SCRIPT\"\n    fi\nelse\n    echo \"Info: Nushell (\'nu\') not found. Skipping dynamic permission setting.\"\nfi\n\nexit 0"
    $bash_hook_content | save $target_hook --force
    print "Making hook executable..."
    ^chmod +x $target_hook
    print "---"
    print "SUCCESS: Post-checkout hook installed. The hook will run after 'git checkout' or 'git clone'."
}
main
