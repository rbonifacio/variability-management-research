package br.unb.cdt.desafioPositivo.model.acesso;

import java.util.Calendar;
import java.util.Date;

import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name="TB_CADASTRO_SOLICITADO")
@DiscriminatorValue("1")
public class CadastroSolicitado extends AcessoUsuario {
	
	private Date cadastroEfetivado;
	private String codigoEfetivacao;
	
	@Override
	public void autenticar(boolean autenticacao) throws ExcecaoAcessoUsuario {
		throw new ExcecaoAcessoUsuario("Nao e possivel autenticar o usuario. Necessario confirmar o cadastro");
	}

	@Override
	public void confirmarCadastro(String token) throws ExcecaoAcessoUsuario {
		if(codigoEfetivacao.equals(token)) {
			cadastroEfetivado = Calendar.getInstance().getTime();
			usuario.setAcessoUsuario(new AcessoAtivo());
		}
		else {
			throw new ExcecaoAcessoUsuario("Codigo de confirmacao do cadastro nao confere");
		}
	}

	public Date getCadastroEfetivado() {
		return cadastroEfetivado;
	}

	public void setCadastroEfetivado(Date cadastroEfetivado) {
		this.cadastroEfetivado = cadastroEfetivado;
	}

	public String getCodigoEfetivacao() {
		return codigoEfetivacao;
	}

	public void setCodigoEfetivacao(String codigoEfetivacao) {
		this.codigoEfetivacao = codigoEfetivacao;
	}

}
