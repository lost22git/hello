when defined(async):
  import ./internal/async
else:
  import ./internal/sync
