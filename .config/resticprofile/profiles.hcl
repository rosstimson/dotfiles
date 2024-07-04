# Global Settings (inherited by all)
# -----------------------------------------------------------------------------

global {
  password-command = "op read 'op://personal/Restic Backup/password'"
  prevent-sleep = true
  priority = "low"
  run-before = [
    "echo  AWS_ACCESS_KEY_ID=\"$(op read 'op://personal/Restic Backup/aws_access_key_id')\" >> {{ env }}",
    "echo  AWS_SECRET_ACCESS_KEY=\"$(op read 'op://personal/Restic Backup/aws_secret_access_key')\" >> {{ env }}",
  ]

  backup = {
    exclude-caches = true
    exclude-file = "~/.config/restic/global-exclude.txt"
    no-error-on-warning = true
    one-file-system = true
    schedule = "weekly"
    schedule-after-network-online = true
    schedule-lock-wait = "2h"
    schedule-permission = "user"
  }
}


# Default Backup - Laptop
# -----------------------------------------------------------------------------

default {
  inherit = "global"
  lock = "/tmp/resticprofile-default.lock"
  repository = "s3:s3.us-east-005.backblazeb2.com/rosstimson-backup/laptop"

  backup = {
    files-from = "~/.config/restic/default-include.txt"
    schedule-log = "~/Library/Logs/resticprofile-default-scheduled.log"
  }

  cache = {
    cleanup = true
  }
}


# External Drive Backup - LaCie Ext Drive with Music and Photos
# -----------------------------------------------------------------------------

ext-storage {
  inherit = "global"
  lock = "/tmp/resticprofile-ext-storage.lock"
  repository = "s3:s3.us-east-005.backblazeb2.com/rosstimson-backup/ext-storage"

  backup = {
    files-from = "~/.config/restic/ext-storage-include.txt"
    schedule-log = "~/Library/Logs/resticprofile-ext-storage-scheduled.log"
  }

  cache = {
    cleanup = true
  }
}
