class LCVariable < Struct.new(:name)
    def to_s
        name.to_s
    end

    def inspect
        to_s
    end

    def callable?
        false
    end

    def reducible?
        false
    end

    def replace(name, replacement)
        if self.name == name
            replacement
        else
            self
        end
    end
end

class LCFunction < Struct.new(:parameter, :body)
    def to_s
        "-> #{parameter} { #{body} }"
    end

    def inspect
        to_s
    end

    def reducible?
        false
    end

    def callable?   
        true
    end

    def replace(name, replacement)
        if parameter == name
            self
        else
            LCFunction.new(parameter, body.replace(name, replacement))
        end
    end

    def call(argument)
        body.replace(parameter, argument)
    end
end

class LCCall < Struct.new(:left, :right)
    def to_s
        "#{left}[#{right}]"
    end

    def inspect
        to_s
    end

    def reducible?
        left.reducible? || right.reducible? || left.callable?
    end

    def callable?
        false
    end

    def reduce 
        if left.reducible?
            LCCall.new(left.reduce, right)
        elsif right.reducible?
            LCCall.new(left, right.reduce)
        else
            left.call(right)
        end
    end
    
    def replace(name, replacement)
        LCCall.new(left.replace(name, replacement), right.replace(name, replacement))
    end
end

