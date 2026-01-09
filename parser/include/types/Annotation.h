#pragma once
#include <algorithm>
#include <optional>
#include <string>


namespace types {
    enum class AnnotationTarget {
        FUNCTION,
        STRUCT,
        ENUM,
        VARIABLE,
        MODULE,
    };

    class Annotation {
    private:
        std::string m_name;
        AnnotationTarget m_target;

    public:
        Annotation(std::string name, const AnnotationTarget target)
            : m_name(std::move(name)), m_target(target) {
        }

        virtual ~Annotation() = default;

        [[nodiscard]] const std::string &name() const { return m_name; }
        [[nodiscard]] AnnotationTarget target() const { return m_target; }
    };

    class ExternalAnnotation : public Annotation {
    private:
        std::string m_library;
        std::optional<std::string> m_externalName;

    public:
        ExternalAnnotation(std::string library,
                           std::optional<std::string> externalName = std::nullopt)
            : Annotation("extern", AnnotationTarget::FUNCTION),
              m_library(std::move(library)), m_externalName(std::move(externalName)) {
        }

        [[nodiscard]] const std::string &library() const { return m_library; }

        [[nodiscard]] const std::optional<std::string> &externalName() const { return m_externalName; }
    };
}
