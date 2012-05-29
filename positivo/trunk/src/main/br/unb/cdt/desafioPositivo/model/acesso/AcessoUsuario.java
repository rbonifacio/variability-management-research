package br.unb.cdt.desafioPositivo.model.acesso;

import javax.persistence.DiscriminatorColumn;
import javax.persistence.DiscriminatorType;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import br.unb.cdt.desafioPositivo.model.Usuario;

@Entity
@Inheritance(strategy=InheritanceType.JOINED)
@Table(name="TB_ACESSO_USUARIO")
@DiscriminatorColumn(name="estadoAcesso",discriminatorType=DiscriminatorType.INTEGER)
public abstract class AcessoUsuario {

	@Id
	@GeneratedValue(strategy=GenerationType.IDENTITY)
	private Long id;
	
	@OneToOne(mappedBy="acessoUsuario")
	protected Usuario usuario;
	
	public abstract void autenticar(boolean autenticacao) throws ExcecaoAcessoUsuario;
	
	public abstract void confirmarCadastro(String token) throws ExcecaoAcessoUsuario; 

	public Usuario getUsuario() {
		return usuario;
	}

	public void setUsuario(Usuario usuario) {
		this.usuario = usuario;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}
}
