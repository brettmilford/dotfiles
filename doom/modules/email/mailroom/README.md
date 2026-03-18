# Mailroom

A Doom Emacs module that wraps mu4e into a tri-panel email client with a
multi-account sidebar, vim-style navigation, and saved searches.

## Prerequisites

Install these tools (checked at startup by `doctor.el`):

| Tool     | Purpose              | Install (NixOS)          |
|----------|----------------------|--------------------------|
| `mu`     | Indexing and search   | `pkgs.mu`                |
| `mbsync` | IMAP sync             | `pkgs.isync`             |
| `msmtp`  | Sending mail          | `pkgs.msmtp`             |

## Setup on a New Machine

### 1. Enable the module

In `doom/init.el`:

```elisp
:email
mu4e
mailroom
```

Run `doom sync`.

### 2. Configure mbsync

Create `~/.mbsyncrc`. Example for Gmail:

```
IMAPAccount personal
Host imap.gmail.com
User you@gmail.com
PassCmd "gpg --quiet --for-your-eyes-only --no-tty --decrypt ~/.authinfo.gpg | grep imap.gmail.com | awk '{print $NF}'"
SSLType IMAPS

IMAPStore personal-remote
Account personal

MaildirStore personal-local
Subfolders Verbatim
Path ~/.mail/personal/
Inbox ~/.mail/personal/INBOX

Channel personal
Far :personal-remote:
Near :personal-local:
Patterns *
Create Both
Expunge Both
SyncState *
```

Repeat for each account, changing the names and credentials.

### 3. Configure msmtp

Create `~/.config/msmtp/config`:

```
defaults
auth           on
tls            on
tls_trust_file /etc/ssl/certs/ca-certificates.crt
logfile        ~/.local/share/msmtp/msmtp.log

account        personal
host           smtp.gmail.com
port           587
from           you@gmail.com
user           you@gmail.com
passwordeval   "gpg --quiet --for-your-eyes-only --no-tty --decrypt ~/.authinfo.gpg | grep smtp.gmail.com | awk '{print $NF}'"

account default : personal
```

### 4. Store credentials

Create `~/.authinfo.gpg` (encrypted with your GPG key):

```
machine imap.gmail.com login you@gmail.com port 993 password YOUR_APP_PASSWORD
machine smtp.gmail.com login you@gmail.com port 587 password YOUR_APP_PASSWORD
```

For Gmail, use an [App Password](https://myaccount.google.com/apppasswords),
not your account password.

### 5. Create the maildir and do initial sync

```bash
mkdir -p ~/.mail/personal
mbsync -a
```

### 6. Initialize and index mu

```bash
mu init --maildir=~/.mail
mu index
```

### 7. Configure accounts for mailroom

Copy the template and fill in your details:

```bash
cp doom/mailroom-accounts.el.template ~/.config/doom/mailroom-accounts.el
```

Edit it with your account info:

```elisp
(set-email-account! "Personal"
  '((mu4e-sent-folder       . "/personal/Sent")
    (mu4e-drafts-folder     . "/personal/Drafts")
    (mu4e-trash-folder      . "/personal/Trash")
    (mu4e-refile-folder     . "/personal/Archive")
    (user-mail-address      . "you@gmail.com")
    (smtpmail-smtp-user     . "you@gmail.com")
    (smtpmail-smtp-server   . "smtp.gmail.com")
    (smtpmail-smtp-service  . 587))
  t) ; t = default account
```

The folder paths (e.g. `/personal/Sent`) must match the maildir structure
created by mbsync. This file is gitignored.

### 8. Launch

In Emacs: `M-x =mailroom`

Standalone: `~/.local/bin/mailroom`

## Keybindings

### All panes

| Key   | Action                |
|-------|-----------------------|
| `h`   | Focus pane left       |
| `l`   | Focus pane right      |
| `/`   | Search current folder |
| `g /` | Search all accounts   |
| `g s` | Jump to saved searches|
| `g S` | Save current search   |
| `g r` | Refresh mail          |

### Sidebar

| Key   | Action              |
|-------|---------------------|
| `j/k` | Navigate            |
| `RET` | Open folder/search  |
| `TAB` | Collapse/expand     |
| `o`   | Next account        |
| `q`   | Quit mailroom       |

### Message list (headers)

| Key   | Action              |
|-------|---------------------|
| `j/k` | Navigate (previews) |
| `c`   | Compose new         |
| `r`   | Reply               |
| `R`   | Reply all           |
| `f`   | Forward             |
| `d`   | Mark for trash      |
| `s`   | Flag/star           |
| `!`   | Mark unread         |
| `x`   | Execute marks       |
| `q`   | Quit mailroom       |

### Message view

| Key   | Action              |
|-------|---------------------|
| `j/k` | Scroll              |
| `J/K` | Next/prev message   |
| `r`   | Reply               |
| `R`   | Reply all           |
| `f`   | Forward             |
| `d`   | Mark for trash      |
| `o`   | Open attachment     |
| `q`   | Back to list        |

## Configuration

Set these in your doom config (before mailroom loads):

```elisp
;; Saved searches shown in sidebar
(setq mailroom-saved-searches
      '((:name "Unread (7d)" :query "flag:unread date:7d..now")
        (:name "Flagged"     :query "flag:flagged")))

;; Auto-refresh interval in seconds (nil to disable)
(setq mailroom-auto-refresh-interval 300)

;; Layout
(setq mailroom-sidebar-width 30
      mailroom-list-width-fraction 0.35)
```

## Troubleshooting

**"mu4e server process ended with exit code 1"**
Run `mu init --maildir=~/.mail && mu index`. The mu database needs
to be created before mu4e can start.

**Sidebar shows no accounts**
Ensure `mailroom-accounts.el` exists and contains `set-email-account!`
calls. Check `M-x describe-variable mu4e-contexts` to verify accounts loaded.

**Unread counts are wrong**
Run `mu index` to re-index, then `g r` in mailroom to refresh.
