// Comprehensive test suite for format validators
// Tests email, UUID, URI, and ISO8601 validators with edge cases
import gleam/string
import gleeunit
import gleeunit/should
import intent/formats

pub fn main() -> Nil {
  gleeunit.main()
}

// =============================================================================
// EMAIL VALIDATION TESTS
// =============================================================================

pub fn validate_email_valid_standard_test() {
  let email = "user@example.com"
  let result = formats.validate_email(email)
  should.equal(result, Ok(Nil))
}

pub fn validate_email_valid_subdomain_test() {
  let email = "user@mail.example.com"
  let result = formats.validate_email(email)
  should.equal(result, Ok(Nil))
}

pub fn validate_email_valid_with_plus_test() {
  let email = "user+tag@example.com"
  let result = formats.validate_email(email)
  should.equal(result, Ok(Nil))
}

pub fn validate_email_valid_with_hyphen_test() {
  let email = "user-name@example.com"
  let result = formats.validate_email(email)
  should.equal(result, Ok(Nil))
}

pub fn validate_email_valid_with_underscore_test() {
  let email = "user_name@example.com"
  let result = formats.validate_email(email)
  should.equal(result, Ok(Nil))
}

pub fn validate_email_valid_with_dots_test() {
  let email = "user.name@example.com"
  let result = formats.validate_email(email)
  should.equal(result, Ok(Nil))
}

pub fn validate_email_empty_local_test() {
  let email = "@example.com"
  let result = formats.validate_email(email)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("local part cannot be empty")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_email_empty_domain_test() {
  let email = "user@"
  let result = formats.validate_email(email)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("domain cannot be empty")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_email_consecutive_dots_test() {
  let email = "user..name@example.com"
  let result = formats.validate_email(email)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("consecutive dots")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_email_starts_with_dot_test() {
  let email = ".user@example.com"
  let result = formats.validate_email(email)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("start or end with a dot")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_email_ends_with_dot_test() {
  let email = "user.@example.com"
  let result = formats.validate_email(email)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("start or end with a dot")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_email_no_at_symbol_test() {
  let email = "userexample.com"
  let result = formats.validate_email(email)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("invalid @ format")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_email_multiple_at_symbols_test() {
  let email = "user@name@example.com"
  let result = formats.validate_email(email)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("invalid @ format")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_email_domain_without_dot_test() {
  let email = "user@example"
  let result = formats.validate_email(email)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("must contain at least one dot")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_email_domain_starts_with_dot_test() {
  let email = "user@.example.com"
  let result = formats.validate_email(email)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("empty label")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_email_domain_label_starts_with_hyphen_test() {
  let email = "user@-example.com"
  let result = formats.validate_email(email)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("invalid labels")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_email_domain_label_ends_with_hyphen_test() {
  let email = "user@example-.com"
  let result = formats.validate_email(email)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("invalid labels")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_email_invalid_special_chars_test() {
  let email = "user!name@example.com"
  let result = formats.validate_email(email)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("invalid characters")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

// =============================================================================
// UUID VALIDATION TESTS
// =============================================================================

pub fn validate_uuid_valid_v4_test() {
  let uuid = "550e8400-e29b-41d4-a716-446655440000"
  let result = formats.validate_uuid(uuid)
  should.equal(result, Ok(Nil))
}

pub fn validate_uuid_valid_v1_test() {
  let uuid = "00000000-0000-1000-8000-000000000000"
  let result = formats.validate_uuid(uuid)
  should.equal(result, Ok(Nil))
}

pub fn validate_uuid_valid_v5_test() {
  let uuid = "550e8400-e29b-51d4-a716-446655440000"
  let result = formats.validate_uuid(uuid)
  should.equal(result, Ok(Nil))
}

pub fn validate_uuid_wrong_segment_count_test() {
  let uuid = "550e8400-e29b-41d4-a716"
  let result = formats.validate_uuid(uuid)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("invalid segment count")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_uuid_wrong_segment_lengths_test() {
  let uuid = "550e8400-e29b-41d-a716-446655440000"
  let result = formats.validate_uuid(uuid)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("segment lengths")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_uuid_non_hex_characters_test() {
  let uuid = "550g8400-e29b-41d4-a716-446655440000"
  let result = formats.validate_uuid(uuid)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("non-hexadecimal")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_uuid_invalid_version_test() {
  let uuid = "550e8400-e29b-61d4-a716-446655440000"
  let result = formats.validate_uuid(uuid)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("invalid UUID version")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_uuid_invalid_variant_test() {
  let uuid = "550e8400-e29b-41d4-0716-446655440000"
  let result = formats.validate_uuid(uuid)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("invalid RFC 4122 variant")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

// =============================================================================
// URI VALIDATION TESTS
// =============================================================================

pub fn validate_uri_valid_https_test() {
  let uri = "https://example.com/path"
  let result = formats.validate_uri(uri)
  should.equal(result, Ok(Nil))
}

pub fn validate_uri_valid_http_test() {
  let uri = "http://example.com"
  let result = formats.validate_uri(uri)
  should.equal(result, Ok(Nil))
}

pub fn validate_uri_valid_ftp_test() {
  let uri = "ftp://ftp.example.com/files"
  let result = formats.validate_uri(uri)
  should.equal(result, Ok(Nil))
}

pub fn validate_uri_missing_scheme_test() {
  let uri = "example.com/path"
  let result = formats.validate_uri(uri)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("missing scheme")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_uri_empty_test() {
  let uri = ""
  let result = formats.validate_uri(uri)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("cannot be empty")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_uri_scheme_starts_with_number_test() {
  let uri = "123://example.com"
  let result = formats.validate_uri(uri)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("must start with a letter")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_uri_empty_scheme_test() {
  let uri = "://example.com"
  let result = formats.validate_uri(uri)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("scheme cannot be empty")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_uri_no_authority_test() {
  let uri = "https://"
  let result = formats.validate_uri(uri)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("no authority after scheme")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

// =============================================================================
// ISO8601 DATETIME VALIDATION TESTS
// =============================================================================

pub fn validate_iso8601_valid_date_only_test() {
  let datetime = "2024-02-08"
  let result = formats.validate_iso8601(datetime)
  should.equal(result, Ok(Nil))
}

pub fn validate_iso8601_valid_datetime_with_t_test() {
  let datetime = "2024-02-08T14:30:00"
  let result = formats.validate_iso8601(datetime)
  should.equal(result, Ok(Nil))
}

pub fn validate_iso8601_leap_year_valid_test() {
  let datetime = "2024-02-29"
  let result = formats.validate_iso8601(datetime)
  should.equal(result, Ok(Nil))
}

pub fn validate_iso8601_non_leap_year_feb_29_test() {
  let datetime = "2023-02-29"
  let result = formats.validate_iso8601(datetime)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("invalid day")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_iso8601_invalid_month_test() {
  let datetime = "2024-13-01"
  let result = formats.validate_iso8601(datetime)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("invalid month")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_iso8601_invalid_day_test() {
  let datetime = "2024-01-32"
  let result = formats.validate_iso8601(datetime)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("invalid day")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_iso8601_century_non_leap_year_test() {
  let datetime = "1900-02-29"
  let result = formats.validate_iso8601(datetime)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("invalid day")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_iso8601_century_divisible_by_400_test() {
  let datetime = "2000-02-29"
  let result = formats.validate_iso8601(datetime)
  should.equal(result, Ok(Nil))
}

pub fn validate_iso8601_april_31_test() {
  let datetime = "2024-04-31"
  let result = formats.validate_iso8601(datetime)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("invalid day")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_iso8601_june_31_test() {
  let datetime = "2024-06-31"
  let result = formats.validate_iso8601(datetime)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("invalid day")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_iso8601_september_31_test() {
  let datetime = "2024-09-31"
  let result = formats.validate_iso8601(datetime)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("invalid day")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_iso8601_november_31_test() {
  let datetime = "2024-11-31"
  let result = formats.validate_iso8601(datetime)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("invalid day")
      |> should.be_true()
    }
    Ok(_) -> should.fail()
  }
}
