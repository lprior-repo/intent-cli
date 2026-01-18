import gleeunit
import gleeunit/should
import intent/config

pub fn main() {
  gleeunit.main()
}

pub fn default_config_test() {
  let cfg = config.default()

  cfg.target_url |> should.equal("")
  cfg.allow_localhost |> should.equal(False)
  cfg.profile |> should.equal("api")
  cfg.output_file |> should.equal("")
  cfg.timeout_ms |> should.equal(30_000)
}

pub fn from_flags_test() {
  let cfg =
    config.from_flags(
      "http://localhost:8080",
      True,
      "cli",
      "results.json",
      60_000,
    )

  cfg.target_url |> should.equal("http://localhost:8080")
  cfg.allow_localhost |> should.equal(True)
  cfg.profile |> should.equal("cli")
  cfg.output_file |> should.equal("results.json")
  cfg.timeout_ms |> should.equal(60_000)
}

pub fn merge_with_flags_override_test() {
  let base = config.default()
  let overrides =
    config.from_flags(
      "http://example.com",
      True,
      "event",
      "out.json",
      45_000,
    )

  let merged = config.merge_with_flags(base, overrides)

  merged.target_url |> should.equal("http://example.com")
  merged.allow_localhost |> should.equal(True)
  merged.profile |> should.equal("event")
  merged.output_file |> should.equal("out.json")
  merged.timeout_ms |> should.equal(45_000)
}

pub fn merge_with_flags_partial_override_test() {
  let base =
    config.from_flags(
      "http://base.com",
      False,
      "api",
      "base.json",
      30_000,
    )

  let overrides =
    config.from_flags(
      "http://override.com",
      False,
      "api",
      "",
      30_000,
    )

  let merged = config.merge_with_flags(base, overrides)

  merged.target_url |> should.equal("http://override.com")
  merged.output_file |> should.equal("base.json")
}

pub fn has_target_test() {
  let without_target = config.default()
  let with_target =
    config.from_flags(
      "http://localhost:8080",
      False,
      "api",
      "",
      30_000,
    )

  config.has_target(without_target) |> should.equal(False)
  config.has_target(with_target) |> should.equal(True)
}

pub fn validate_target_required_success_test() {
  let cfg =
    config.from_flags(
      "http://localhost:8080",
      False,
      "api",
      "",
      30_000,
    )

  config.validate_target_required(cfg) |> should.be_ok()
}

pub fn validate_target_required_failure_test() {
  let cfg = config.default()

  config.validate_target_required(cfg) |> should.be_error()
}

pub fn is_localhost_allowed_test() {
  let allowed =
    config.from_flags(
      "http://localhost:8080",
      True,
      "api",
      "",
      30_000,
    )

  let not_allowed = config.default()

  config.is_localhost_allowed(allowed) |> should.equal(True)
  config.is_localhost_allowed(not_allowed) |> should.equal(False)
}
